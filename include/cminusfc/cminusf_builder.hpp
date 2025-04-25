#pragma once

#include "BasicBlock.hpp"
#include "Constant.hpp"
#include "Function.hpp"
#include "IRBuilder.hpp"
#include "Module.hpp"
#include "Type.hpp"
#include "ast.hpp"

#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <vector>

class Scope {
  public:
    void enter() { inner.emplace_back(); }
    void exit() { inner.pop_back(); }
    bool in_global() const { return inner.size() == 1; }

    bool push(const std::string &name, Value *val) {
        return inner.back().emplace(name, val).second;
    }

    Value *find(const std::string &name) {
        for (auto it = inner.rbegin(); it != inner.rend(); ++it) {
            auto f = it->find(name);
            if (f != it->end())
                return f->second;
        }
        assert(false && "Name not found in scope");
    }

  private:
    std::vector<std::map<std::string, Value *>> inner{{}}; // start with global
};

class CminusfBuilder : public ASTVisitor {
  public:
    CminusfBuilder();
    std::unique_ptr<Module> getModule();

    // helper to unify int/float
    static bool promote(IRBuilder *b, Value **l, Value **r);

  private:
    // ASTVisitor
    Value *visit(ASTProgram &) override;
    Value *visit(ASTNum &) override;
    Value *visit(ASTVarDeclaration &) override;
    Value *visit(ASTFunDeclaration &) override;
    Value *visit(ASTParam &) override;
    Value *visit(ASTCompoundStmt &) override;
    Value *visit(ASTExpressionStmt &) override;
    Value *visit(ASTSelectionStmt &) override;
    Value *visit(ASTIterationStmt &) override;
    Value *visit(ASTReturnStmt &) override;
    Value *visit(ASTAssignExpression &) override;
    Value *visit(ASTSimpleExpression &) override;
    Value *visit(ASTAdditiveExpression &) override;
    Value *visit(ASTVar &) override;
    Value *visit(ASTTerm &) override;
    Value *visit(ASTCall &) override;

    // core state
    std::unique_ptr<Module> module;
    std::unique_ptr<IRBuilder> builder;
    Scope scope;
    Function *currentFunc = nullptr;

    // NEW: lvalue context flag
    bool require_lvalue = false;
};