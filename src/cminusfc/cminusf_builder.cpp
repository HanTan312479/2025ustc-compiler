#include "cminusf_builder.hpp"
#include <string>

// macros for constants
#define CONST_INT(i) ConstantInt::get(i, module.get())
#define CONST_FP(f) ConstantFP::get((float)f, module.get())
#define CONST_ZERO(t) ConstantZero::get(t, module.get())

// promote: cast integer operand to match the other's type
bool CminusfBuilder::promote(IRBuilder *b, Value **l, Value **r) {
    Type *lt = (*l)->get_type();
    Type *rt = (*r)->get_type();
    bool both_int = lt->is_integer_type() && rt->is_integer_type();
    if (lt != rt) {
        if (lt->is_integer_type()) {
            *l = b->create_sitofp(*l, rt);
        } else {
            *r = b->create_sitofp(*r, lt);
        }
    }
    return both_int;
}

CminusfBuilder::CminusfBuilder() {
    module = std::make_unique<Module>();
    builder = std::make_unique<IRBuilder>(nullptr, module.get());

    // register built-ins
    auto *I32 = module->get_int32_type();
    auto *F32 = module->get_float_type();
    auto *VoidT = module->get_void_type();
    Function *fn;

    fn = Function::create(FunctionType::get(I32, {}), "input", module.get());
    scope.push("input", fn);
    fn = Function::create(FunctionType::get(VoidT, {I32}), "output",
                          module.get());
    scope.push("output", fn);
    fn = Function::create(FunctionType::get(VoidT, {F32}), "outputFloat",
                          module.get());
    scope.push("outputFloat", fn);
    fn = Function::create(FunctionType::get(VoidT, {}), "neg_idx_except",
                          module.get());
    scope.push("neg_idx_except", fn);
}

std::unique_ptr<Module> CminusfBuilder::getModule() {
    return std::move(module);
}

Value *CminusfBuilder::visit(ASTProgram &node) {
    Value *last = nullptr;
    for (auto &d : node.declarations)
        last = d->accept(*this);
    return last;
}

Value *CminusfBuilder::visit(ASTNum &node) {
    if (node.type == TYPE_INT)
        return CONST_INT(node.i_val);
    else
        return CONST_FP(node.f_val);
}

Value *CminusfBuilder::visit(ASTVarDeclaration &node) {
    Type *base;
    if (node.type == TYPE_INT) {
        base = module->get_int32_type();
    } else {
        base = module->get_float_type();
    }
    Value *alloc = nullptr;
    if (scope.in_global()) {
        if (!node.num) {
            alloc = GlobalVariable::create(node.id, module.get(), base, false,
                                           CONST_ZERO(base));
        } else {
            auto *arrTy = ArrayType::get(base, node.num->i_val);
            alloc = GlobalVariable::create(node.id, module.get(), arrTy, false,
                                           CONST_ZERO(arrTy));
        }
    } else {
        if (!node.num) {
            alloc = builder->create_alloca(base);
        } else {
            auto *arrTy = ArrayType::get(base, node.num->i_val);
            alloc = builder->create_alloca(arrTy);
        }
    }
    scope.push(node.id, alloc);
    return alloc;
}

Value *CminusfBuilder::visit(ASTFunDeclaration &node) {
    Type *retTy;
    if (node.type == TYPE_INT)
        retTy = module->get_int32_type();
    else if (node.type == TYPE_FLOAT)
        retTy = module->get_float_type();
    else
        retTy = module->get_void_type();

    bool isMain = (node.id == "main");
    if (isMain && retTy->is_void_type()) {
        retTy = module->get_int32_type();
    }

    std::vector<Type *> params;
    for (auto &p : node.params) {
        if (p->isarray) {
            if (p->type == TYPE_INT)
                params.push_back(module->get_int32_ptr_type());
            else
                params.push_back(module->get_float_ptr_type());
        } else {
            if (p->type == TYPE_INT)
                params.push_back(module->get_int32_type());
            else
                params.push_back(module->get_float_type());
        }
    }

    auto *fnTy = FunctionType::get(retTy, params);
    auto *fn = Function::create(fnTy, node.id, module.get());
    scope.push(node.id, fn);
    currentFunc = fn;

    scope.enter();
    auto *bb = BasicBlock::create(module.get(), "entry", fn);
    builder->set_insert_point(bb);

    unsigned idx = 0;
    for (auto &arg : fn->get_args()) {
        Value *slot = builder->create_alloca(params[idx]);
        builder->create_store(&arg, slot);
        scope.push(node.params[idx]->id, slot);
        ++idx;
    }

    node.compound_stmt->accept(*this);

    if (!builder->get_insert_block()->is_terminated()) {
        if (retTy->is_integer_type()) {
            builder->create_ret(CONST_INT(0));
        } else if (retTy->is_void_type()) {
            builder->create_void_ret();
        } else {
            builder->create_ret(CONST_FP(0.0f));
        }
    }

    scope.exit();
    currentFunc = nullptr;
    return nullptr;
}

Value *CminusfBuilder::visit(ASTParam & /*node*/) { return nullptr; }

Value *CminusfBuilder::visit(ASTCompoundStmt &node) {
    scope.enter();
    Value *last = nullptr;
    for (auto &d : node.local_declarations)
        d->accept(*this);
    for (auto &s : node.statement_list) {
        last = s->accept(*this);
        if (builder->get_insert_block()->is_terminated())
            break;
    }
    scope.exit();
    return last;
}

Value *CminusfBuilder::visit(ASTExpressionStmt &node) {
    return node.expression ? node.expression->accept(*this) : nullptr;
}

Value *CminusfBuilder::visit(ASTSelectionStmt &node) {
    Value *cv = node.expression->accept(*this);
    if (cv->get_type()->is_pointer_type())
        cv = builder->create_load(cv);
    Value *cmp = nullptr;
    if (cv->get_type()->is_integer_type())
        cmp = builder->create_icmp_ne(cv, CONST_INT(0));
    else
        cmp = builder->create_fcmp_ne(cv, CONST_FP(0.0f));

    auto *tb = BasicBlock::create(module.get(), "", currentFunc);
    auto *mb = BasicBlock::create(module.get(), "", currentFunc);
    BasicBlock *fb = nullptr;

    if (node.else_statement) {
        fb = BasicBlock::create(module.get(), "", currentFunc);
        builder->create_cond_br(cmp, tb, fb);
    } else {
        builder->create_cond_br(cmp, tb, mb);
    }

    builder->set_insert_point(tb);
    node.if_statement->accept(*this);
    if (!builder->get_insert_block()->is_terminated())
        builder->create_br(mb);

    if (fb) {
        builder->set_insert_point(fb);
        node.else_statement->accept(*this);
        if (!builder->get_insert_block()->is_terminated())
            builder->create_br(mb);
    }

    builder->set_insert_point(mb);
    return nullptr;
}

Value *CminusfBuilder::visit(ASTIterationStmt &node) {
    // Create basic blocks for loop condition, body, and after
    auto *condBB = BasicBlock::create(module.get(), "", currentFunc);
    auto *bodyBB = BasicBlock::create(module.get(), "", currentFunc);
    auto *afterBB = BasicBlock::create(module.get(), "", currentFunc);

    // Jump to condition check
    builder->create_br(condBB);
    builder->set_insert_point(condBB);

    // Evaluate loop condition expression
    Value *condVal = node.expression->accept(*this);
    // If it yields a pointer, load until we have a concrete value
    while (condVal->get_type()->is_pointer_type()) {
        condVal = builder->create_load(condVal);
    }

    // Determine comparison kind based on the concrete value type
    Value *cmp = nullptr;
    Type *ct = condVal->get_type();
    if (ct->is_int1_type()) {
        // Already a boolean (i1)
        cmp = condVal;
    } else if (ct->is_integer_type()) {
        // Any other integer width
        cmp = builder->create_icmp_ne(condVal, CONST_INT(0));
    } else if (ct->is_float_type()) {
        // Floating-point condition
        cmp = builder->create_fcmp_ne(condVal, CONST_FP(0.0f));
    } else {
        assert(false && "Unsupported type in loop condition");
    }

    // Branch on the condition
    builder->create_cond_br(cmp, bodyBB, afterBB);

    // Loop body
    builder->set_insert_point(bodyBB);
    node.statement->accept(*this);
    if (!builder->get_insert_block()->is_terminated()) {
        builder->create_br(condBB);
    }

    // After loop
    builder->set_insert_point(afterBB);
    return nullptr;
}

Value *CminusfBuilder::visit(ASTReturnStmt &node) {
    if (!node.expression) {
        if (currentFunc->get_name() == "main") {
            builder->create_ret(CONST_INT(0));
        } else {
            builder->create_void_ret();
        }
    } else {
        Value *v = node.expression->accept(*this);
        if (v->get_type()->is_pointer_type())
            v = builder->create_load(v);
        Type *rt = currentFunc->get_function_type()->get_return_type();
        if (currentFunc->get_name() == "main" && rt->is_integer_type()) {
            builder->create_ret(CONST_INT(0));
        } else {
            if (v->get_type() != rt) {
                if (rt->is_integer_type())
                    v = builder->create_fptosi(v, module->get_int32_type());
                else
                    v = builder->create_sitofp(v, module->get_float_type());
            }
            builder->create_ret(v);
        }
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTVar &node) {
    Value *addr = scope.find(node.id);

    if (!node.expression) {
        auto *elemTy = addr->get_type()->get_pointer_element_type();
        if (elemTy->is_array_type()) {
            return builder->create_gep(addr, {CONST_INT(0), CONST_INT(0)});
        }

        return addr;
    }

    Value *idx = node.expression->accept(*this);
    if (idx->get_type()->is_pointer_type()) {
        idx = builder->create_load(idx);
    }
    if (idx->get_type()->is_float_type()) {
        idx = builder->create_fptosi(idx, module->get_int32_type());
    }

    if (addr->get_type()->get_pointer_element_type()->is_pointer_type()) {
        addr = builder->create_load(addr);
    }

    Value *zero = CONST_INT(0);
    Value *isNeg = builder->create_icmp_lt(idx, zero);
    auto *exBB = BasicBlock::create(module.get(), "", currentFunc);
    auto *okBB = BasicBlock::create(module.get(), "", currentFunc);
    builder->create_cond_br(isNeg, exBB, okBB);

    builder->set_insert_point(exBB);
    auto *errFn = static_cast<Function *>(scope.find("neg_idx_except"));
    builder->create_call(errFn, {});
    builder->create_br(okBB);

    builder->set_insert_point(okBB);

    // decide indices based on pointer element type
    Type *pte = addr->get_type()->get_pointer_element_type();
    if (pte->is_array_type()) {
        // addr is T[N]*, use two indices to get element pointer T*
        return builder->create_gep(addr, {zero, idx});
    } else {
        // addr is T* (parameter or decayed array), use single index
        return builder->create_gep(addr, {idx});
    }
}

Value *CminusfBuilder::visit(ASTAssignExpression &node) {
    Value *rhs = node.expression->accept(*this);
    if (rhs->get_type()->is_pointer_type())
        rhs = builder->create_load(rhs);
    Value *addr = node.var->accept(*this);
    Type *et = addr->get_type()->get_pointer_element_type();
    if (rhs->get_type()->is_float_type() && et->is_integer_type()) {
        rhs = builder->create_fptosi(rhs, module->get_int32_type());
    } else if (rhs->get_type()->is_integer_type() && et->is_float_type()) {
        rhs = builder->create_sitofp(rhs, module->get_float_type());
    }
    // 5) 一定要 store
    builder->create_store(rhs, addr);
    return rhs;
}

Value *CminusfBuilder::visit(ASTSimpleExpression &node) {
    Value *l = node.additive_expression_l->accept(*this);
    if (!node.additive_expression_r)
        return l;

    Value *r = node.additive_expression_r->accept(*this);
    // load pointers
    if (l->get_type()->is_pointer_type())
        l = builder->create_load(l);
    if (r->get_type()->is_pointer_type())
        r = builder->create_load(r);
    bool is_int = promote(builder.get(), &l, &r);
    Value *cmp = nullptr;

    if (is_int) {
        switch (node.op) {
        case OP_LT:
            cmp = builder->create_icmp_lt(l, r);
            break;
        case OP_LE:
            cmp = builder->create_icmp_le(l, r);
            break;
        case OP_GT:
            cmp = builder->create_icmp_gt(l, r);
            break;
        case OP_GE:
            cmp = builder->create_icmp_ge(l, r);
            break;
        case OP_EQ:
            cmp = builder->create_icmp_eq(l, r);
            break;
        case OP_NEQ:
            cmp = builder->create_icmp_ne(l, r);
            break;
        }
    } else {
        switch (node.op) {
        case OP_LT:
            cmp = builder->create_fcmp_lt(l, r);
            break;
        case OP_LE:
            cmp = builder->create_fcmp_le(l, r);
            break;
        case OP_GT:
            cmp = builder->create_fcmp_gt(l, r);
            break;
        case OP_GE:
            cmp = builder->create_fcmp_ge(l, r);
            break;
        case OP_EQ:
            cmp = builder->create_fcmp_eq(l, r);
            break;
        case OP_NEQ:
            cmp = builder->create_fcmp_ne(l, r);
            break;
        }
    }
    return builder->create_zext(cmp, module->get_int32_type());
}

Value *CminusfBuilder::visit(ASTAdditiveExpression &node) {
    if (!node.additive_expression)
        return node.term->accept(*this);

    Value *l = node.additive_expression->accept(*this);
    if (l->get_type()->is_pointer_type())
        l = builder->create_load(l);

    Value *r = node.term->accept(*this);
    if (r->get_type()->is_pointer_type())
        r = builder->create_load(r);

    bool is_int = promote(builder.get(), &l, &r);
    Value *res = nullptr;
    if (node.op == OP_PLUS) {
        if (is_int)
            res = builder->create_iadd(l, r);
        else
            res = builder->create_fadd(l, r);
    } else {
        if (is_int)
            res = builder->create_isub(l, r);
        else
            res = builder->create_fsub(l, r);
    }
    return res;
}

Value *CminusfBuilder::visit(ASTTerm &node) {
    if (!node.term)
        return node.factor->accept(*this);

    Value *l = node.term->accept(*this);
    if (l->get_type()->is_pointer_type())
        l = builder->create_load(l);

    Value *r = node.factor->accept(*this);
    if (r->get_type()->is_pointer_type())
        r = builder->create_load(r);

    bool is_int = promote(builder.get(), &l, &r);
    Value *res = nullptr;
    if (node.op == OP_MUL) {
        if (is_int)
            res = builder->create_imul(l, r);
        else
            res = builder->create_fmul(l, r);
    } else {
        if (is_int)
            res = builder->create_isdiv(l, r);
        else
            res = builder->create_fdiv(l, r);
    }
    return res;
}

Value *CminusfBuilder::visit(ASTCall &node) {
    auto *fn = dynamic_cast<Function *>(scope.find(node.id));
    assert(fn && "function not found");

    Type *intTy = module->get_int32_type();
    Type *floatTy = module->get_float_type();

    auto *fnTy = fn->get_function_type();

    std::vector<Value *> args;
    unsigned idx = 0;
    for (auto &argExpr : node.args) {
        Value *v = argExpr->accept(*this);

        Type *paramTy = fnTy->get_param_type(idx);

        if (v->get_type()->is_pointer_type() && v->get_type() != paramTy) {
            v = builder->create_load(v);
        }
        if (!paramTy->is_pointer_type() && v->get_type() != paramTy) {
            if (paramTy->is_integer_type()) {
                // float -> int
                v = builder->create_fptosi(v, intTy);
            } else {

                v = builder->create_sitofp(v, floatTy);
            }
        }

        args.push_back(v);
        ++idx;
    }

    return builder->create_call(fn, args);
}