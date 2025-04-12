import { ClassExp, ProcExp, CExp, Exp, Program, makeProcExp, makeVarDecl, makeIfExp, IfExp, BoolExp, Binding, makeAppExp, makeBoolExp, makeLitExp, makePrimOp, makeVarRef, isExp, isProgram, makeProgram, isDefineExp, makeDefineExp, isBoolExp, isNumExp, isVarRef, isProcExp, isAppExp, isPrimOp, isLetExp, makeLetExp, isIfExp, makeBinding, isLitExp, isClassExp, makeClassExp } from "./L3-ast";
import { Result, makeFailure, bind, mapResult, makeOk, safe2 } from "../shared/result";
import { isEmpty, map, zipWith } from "ramda";
import { makeSymbolSExp } from "./L3-value";

/*
Purpose: Transform ClassExp to ProcExp
Signature: class2proc(classExp)
Type: ClassExp => ProcExp
*/
export const class2proc = (exp: ClassExp): ProcExp =>
    makeProcExp(exp.fields, [makeProcExp([makeVarDecl('msg')], [buildIfExp(exp.methods)])]);

const first = <T>(x: T[]): T => x[0];
const rest = <T>(x: T[]): T[] => x.slice(1);

const buildIfExp = (bindings: Binding[]): IfExp | BoolExp =>
    !isEmpty(bindings) ?
            makeIfExp(makeAppExp(makePrimOp('eq?'),[makeVarRef('msg'), makeLitExp(makeSymbolSExp(first(bindings).var.var))]),
                makeAppExp(first(bindings).val,[]),
                buildIfExp(rest(bindings))) :
        makeBoolExp(false);

/*
Purpose: Transform all class forms in the given AST to procs
Signature: lexTransform(AST)
Type: [Exp | Program] => Result<Exp | Program>
*/
export const lexTransform = (exp: Exp | Program): Result<Exp | Program> =>
    isProgram(exp) ? bind(mapResult(lexTransformExp, exp.exps), (exps: Exp[]) => makeOk(makeProgram(exps))) :
    isExp(exp) ? lexTransformExp(exp) :
    makeFailure("Never");

const lexTransformExp = (exp: Exp): Result<Exp> =>
    isDefineExp(exp) ? bind(lexTransformCExp(exp.val), (val: CExp) => makeOk(makeDefineExp(exp.var, val))) :
    lexTransformCExp(exp);


const lexTransformCExp = (exp: CExp): Result<CExp> =>
    isBoolExp(exp) ? makeOk(exp) :
    isNumExp(exp) ? makeOk(exp) :
    isVarRef(exp) ? makeOk(exp) :
    isLitExp(exp) ? makeOk(exp) :
    isProcExp(exp) ? bind(mapResult(lexTransformCExp, exp.body), (body: CExp[]) => 
        makeOk(makeProcExp(exp.args, body))) :
    isAppExp(exp) ? safe2((rator: CExp, rands: CExp[]) => makeOk(makeAppExp(rator, rands)))
        (lexTransformCExp(exp.rator), mapResult(lexTransformCExp, exp.rands)) :
    isPrimOp(exp) ? makeOk(exp) :
    isIfExp(exp) ? bind(lexTransformCExp(exp.test), (test: CExp) => 
        bind(lexTransformCExp(exp.then), (then: CExp) => 
            bind(lexTransformCExp(exp.alt), (alt: CExp) => 
                makeOk(makeIfExp(test, then, alt))))):
    isLetExp(exp) ? safe2((vals: CExp[], body: CExp[]) => 
        makeOk(makeLetExp(zipWith(makeBinding, map(binding => binding.var.var, exp.bindings), vals), body)))
            (mapResult((binding: Binding ) => 
                lexTransformCExp(binding.val), exp.bindings), mapResult(lexTransformCExp, exp.body)) :
    isClassExp(exp) ? bind(mapResult((binding: Binding ) => lexTransformCExp(binding.val), exp.methods),
                (vals: CExp[]) => makeOk(
                    class2proc(
                        makeClassExp(exp.fields, 
                         zipWith(makeBinding,map(binding => binding.var.var, exp.methods), vals))))) : 
    makeFailure(`Unexpected expression in lexTransformCExp ${exp.tag}`);    