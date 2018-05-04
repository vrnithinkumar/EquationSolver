namespace EquationSolver
module EquationSolver =
    open FSharp.Data
    open EquationParser
    open PrettyPrinter
    open TransformExpreesionTree
    open EquationEvaluation

    let private _evaluateTree jsonInfo = 
        let paresedTree = CreateExpressionTree jsonInfo
        let initialTree = PrettyPrintTree(paresedTree)
        let tt = TransformExpressionTree(paresedTree)
        let transformed = PrettyPrintTree(tt)
        EvaluateTree(tt)

    let ParseFromStringThenEvaluate jsonData =
        let jsonInfo = JsonValue.Parse(jsonData)
        _evaluateTree jsonInfo

    let ParseFromFileThenEvaluate jsonFile =
        let jsonInfo = JsonValue.Load(__SOURCE_DIRECTORY__ + jsonFile)
        _evaluateTree jsonInfo
