// L3-eval.ts
// Evaluator with Environments model

import { map, zipWith } from "ramda";
import { isBoolExp, isCExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef,
         isAppExp, isDefineExp, isIfExp, isLetExp, isProcExp,
         Binding, VarDecl, CExp, Exp, IfExp, LetExp, ProcExp, Program,
         parseL3Exp,  DefineExp,
         isClassExp,
         ClassExp,
         makeClassExp,
         makeBinding,
         makeProcExp} from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeExtEnv, Env } from "./L3-env-env";
import { Object, isClosure, makeClosureEnv, Closure, Value, makeClassEnv, isClass, isObject, Class, makeObjectEnv, isSymbolSExp, SymbolSExp, makeSymbolSExp } from "./L3-value";
import { applyPrimitive } from "./evalPrimitive";
import { allT, first, rest, isEmpty, isNonEmptyList } from "../shared/list";
import { Result, makeOk, makeFailure, bind, mapResult, isFailure, isOk } from "../shared/result";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";
import { isEmptyEnv } from "./L3-env-sub";

// ========================================================
// Eval functions

const applicativeEval = (exp: CExp, env: Env): Result<Value> =>
    isNumExp(exp) ? makeOk(exp.val) :
    isBoolExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? makeOk(exp.val) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isLetExp(exp) ? evalLet(exp, env) :
    isClassExp(exp) ? evalClassExp(exp, env) :
    isAppExp(exp) ? bind(applicativeEval(exp.rator, env),
                      (proc: Value) =>
                        bind(mapResult((rand: CExp) => 
                           applicativeEval(rand, env), exp.rands),
                              (args: Value[]) =>
                                 applyProcedure(proc, args))) :
    makeFailure('"let" not supported (yet)');

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(applicativeEval(exp.test, env), (test: Value) => 
            isTrueValue(test) ? applicativeEval(exp.then, env) : 
            applicativeEval(exp.alt, env));

const evalProc = (exp: ProcExp, env: Env): Result<Closure> =>
    makeOk(makeClosureEnv(exp.args, exp.body, env));

const evalClassExp = (exp:ClassExp, env:Env): Result<Value> => {
    const fields = exp.fields;
    const methods = exp.methods;
    return makeOk(makeClassEnv(fields, methods,env));
}

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value, args: Value[]): Result<Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args) :
    isClass(proc) ? applyClass(proc, args) :
    isObject(proc) ? applyObject(proc, args) :
    makeFailure(`Bad procedure ${format(proc)}`);

const applyClosure = (proc: Closure, args: Value[]): Result<Value> => {
    const vars:string[] = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env));
}

const applyClass = (proc:Class, args:Value[]): Result<Value> => {
    if (args.length !== proc.fields.length) {
        return makeFailure("Error: Number of Arguments is Incompatible!");
    } else {
        const variables: string[] = proc.fields.map((x: VarDecl) => x.var);
        return makeOk(makeObjectEnv(proc.methods, makeExtEnv(variables, args, proc.env)));
    }
}

const applyObject = (proc: Object, args: Value[]): Result<Value> => {
    const m: Value = args[0];
    if (isSymbolSExp(m)) {
        const methodName: SymbolSExp = m;
        const methods: Binding[] = proc.methods.filter((x: Binding) => x.var.var === methodName.val);
        if (isEmpty(methods)) {
            return makeFailure("Unrecognized method: " + methodName.val);
        }
        const mayMethod: CExp = methods[0].val;
        if (isProcExp(mayMethod)) {
            // We want to be here!
            const method: ProcExp = mayMethod;
            const close: Closure = makeClosureEnv(method.args, method.body, proc.env);
            return applyClosure(close, args.slice(1));
        }
        else {
            return makeFailure("Error: not a function!");
        }
    }
    else {
        return makeFailure("Error: does not recognize method's name!");
    }
 }

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: Exp[], env: Env): Result<Value> =>
    isNonEmptyList<Exp>(seq) ? evalCExps(first(seq), rest(seq), env) : 
    makeFailure("Empty sequence");
    
const evalCExps = (first: Exp, rest: Exp[], env: Env): Result<Value> =>
    isDefineExp(first) ? evalDefineExps(first, rest, env) :
    isCExp(first) && isEmpty(rest) ? applicativeEval(first, env) :
    isCExp(first) ? bind(applicativeEval(first, env), _ => evalSequence(rest, env)) :
    first;
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: DefineExp, exps: Exp[], env: Env): Result<Value> =>
    bind(applicativeEval(def.val, env), (rhs: Value) => 
            evalSequence(exps, makeExtEnv([def.var.var], [rhs], env)));


// Main program
export const evalL3program = (program: Program): Result<Value> =>
    evalSequence(program.exps, makeEmptyEnv());

export const evalParse = (s: string): Result<Value> =>
    bind(p(s), (x) => 
        bind(parseL3Exp(x), (exp: Exp) =>
            evalSequence([exp], makeEmptyEnv())));

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env): Result<Value> => {
    const vals  = mapResult((v: CExp) => 
        applicativeEval(v, env), map((b: Binding) => b.val, exp.bindings));
    const vars = map((b: Binding) => b.var.var, exp.bindings);
    return bind(vals, (vals: Value[]) => 
        evalSequence(exp.body, makeExtEnv(vars, vals, env)));
}
