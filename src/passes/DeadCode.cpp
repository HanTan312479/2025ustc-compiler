#include "DeadCode.hpp"
#include "logging.hpp"
#include <vector>

// 处理流程：两趟处理，mark 标记有用变量，sweep 删除无用指令
void DeadCode::run() {
    bool changed{};
    func_info->run();
    do {
        changed = false;
        for (auto &F : m_->get_functions()) {
            auto func = &F;
            changed |= clear_basic_blocks(func);
            mark(func);
            changed |= sweep(func);
        }
        sweep_globally();
    } while (changed);
    LOG_INFO << "dead code pass erased " << ins_count << " instructions";
}

bool DeadCode::clear_basic_blocks(Function *func) {
    bool changed = false;
    std::vector<BasicBlock *> to_erase;
    for (auto &bb : func->get_basic_blocks()) {
        if (bb.get_pre_basic_blocks().empty() &&
            &bb != func->get_entry_block()) {
            to_erase.push_back(&bb);
            changed = true;
        }
    }
    for (auto bb : to_erase) {
        bb->erase_from_parent();
    }
    return changed;
}

void DeadCode::mark(Function *func) {
    work_list.clear();
    marked.clear();
    // 标记所有关键指令
    for (auto &bb : func->get_basic_blocks()) {
        for (auto &ins : bb.get_instructions()) {
            if (is_critical(&ins)) {
                marked[&ins] = true;
                work_list.push_back(&ins);
            }
        }
    }
    // 工作列表算法，标记所有依赖关键指令的指令
    while (!work_list.empty()) {
        auto ins = work_list.front();
        work_list.pop_front();
        mark(ins);
    }
}

void DeadCode::mark(Instruction *ins) {
    for (auto op : ins->get_operands()) {
        auto def = dynamic_cast<Instruction *>(op);
        if (def == nullptr)
            continue;
        if (marked[def])
            continue;
        if (def->get_function() != ins->get_function())
            continue;
        marked[def] = true;
        work_list.push_back(def);
    }
}

bool DeadCode::sweep(Function *func) {
    std::unordered_set<Instruction *> wait_del;
    // 遍历函数中的所有基本块和指令
    for (auto &bb : func->get_basic_blocks()) {
        for (auto &ins : bb.get_instructions()) {
            // 如果指令未被标记为有用，则加入待删除集合
            if (!marked[&ins]) {
                wait_del.insert(&ins);
            }
        }
    }
    // 删除待删除集合中的指令
    for (auto ins : wait_del) {
        // 移除操作数的引用
        for (size_t i = 0; i < ins->get_num_operand(); ++i) {
            if (auto op = ins->get_operand(i)) {
                op->remove_use(ins, i);
            }
        }
        // 增加删除的指令计数
        ins_count++;
        // 通过父基本块删除指令
        ins->get_parent()->remove_instr(ins);
    }
    return !wait_del.empty();
}

bool DeadCode::is_critical(Instruction *ins) {
    // 判断指令是否关键（即不能被删除）
    if (ins->is_call()) {
        // 函数调用：如果函数有副作用（非纯函数），则关键
        auto called_func = dynamic_cast<Function *>(ins->get_operand(0));
        if (called_func && !func_info->is_pure_function(called_func)) {
            return true;
        }
    } else if (ins->is_store()) {
        // 存储指令：通常有副作用，视为关键
        return true;
    } else if (ins->is_ret()) {
        // 返回指令：关键
        return true;
    } else if (ins->is_br()) {
        // 分支指令：影响控制流，视为关键
        return true;
    } else if (ins->is_phi()) {
        // PHI 指令：涉及控制流合并，视为关键
        return true;
    } else if (!ins->get_use_list().empty()) {
        // 如果指令的结果被其他指令使用，则关键
        return true;
    }
    return false;
}

void DeadCode::sweep_globally() {
    std::vector<Function *> unused_funcs;
    std::vector<GlobalVariable *> unused_globals;
    for (auto &f : m_->get_functions()) {
        if (f.get_use_list().empty() && f.get_name() != "main") {
            unused_funcs.push_back(&f);
        }
    }
    for (auto &g : m_->get_global_variable()) {
        if (g.get_use_list().empty()) {
            unused_globals.push_back(&g);
        }
    }
    for (auto func : unused_funcs) {
        m_->get_functions().erase(func->getIterator());
    }
    for (auto glob : unused_globals) {
        m_->get_global_variable().erase(glob->getIterator());
    }
}