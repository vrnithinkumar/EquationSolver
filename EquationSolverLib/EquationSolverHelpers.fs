namespace EquationSolver
module EquationParser =
    open EquationSolverTypes 
    open FSharp.Data

    let ParseOperation value =
        match value with
        | "add"       -> Add
        | "subtract"  -> Subtract
        | "multiply"  -> Multiply
        | "divide"    -> Divide
        | "equal"     -> Equal
        | _           -> NotSupported

    let private _findOp (jsonValue : JsonValue) = 
        jsonValue.AsString()
        |> ParseOperation

    let rec CreateExpressionTree (jsonValue : JsonValue) = 
        match jsonValue with
        | JsonValue.String varRef  -> ExpressionNode.Variable varRef
        | JsonValue.Number value   -> ExpressionNode.Value value
        | JsonValue.Record properties
            -> ExpressionNode.Node (
                snd properties.[0] |> _findOp, 
                snd properties.[1] |> CreateExpressionTree,
                snd properties.[2] |> CreateExpressionTree)
        | _ -> failwith "Unable to create tree ! \n Not supported JSON foramt"

module PrettyPrinter =
    open EquationSolverTypes
    let PrettyFormattedOperation op =
        match op with
        | Add       -> "+"
        | Subtract -> "-"
        | Multiply  -> "*"
        | Divide    -> "/"
        | Equal     -> "="
    
    let private _appendToStringBuilder (builder : System.Text.StringBuilder, value : string) =
        builder.Append value |> ignore
        
    let rec private _prettyPrintExpreesionTreeRec (tree : ExpressionNode, printBracket : bool, builder : System.Text.StringBuilder) =
        match tree with
        | ExpressionNode.Value value  -> _appendToStringBuilder(builder, value |> string)
        | ExpressionNode.Variable var -> _appendToStringBuilder(builder, var)
        | ExpressionNode.Node (op, lhs, rhs) ->
            let printChildInBracket = op <> Operation.Equal
            if printBracket then _appendToStringBuilder(builder, "(")
            _prettyPrintExpreesionTreeRec(lhs, printChildInBracket, builder) 
            _appendToStringBuilder(builder, PrettyFormattedOperation op)
            _prettyPrintExpreesionTreeRec(rhs, printChildInBracket, builder)
            if printBracket then _appendToStringBuilder(builder, ")")
    
    let PrettyPrintTree(tree : ExpressionNode) = 
        let sb = new System.Text.StringBuilder()
        _prettyPrintExpreesionTreeRec(tree, false, sb)
        let expression = sb.ToString()
        printfn "%s" expression
        expression

module TransformExpreesionTree =
    open EquationSolverTypes

    let private _inverseOperation op =
        match op with
        | Add       -> Subtract
        | Subtract  -> Add
        | Multiply  -> Divide
        | Divide    -> Multiply

    let rec private _doesThisNodeContainsVariable (tree : ExpressionNode) =
        match tree with
        | ExpressionNode.Value value  -> false
        | ExpressionNode.Variable var -> true
        | ExpressionNode.Node (op, lhs, rhs) 
            -> _doesThisNodeContainsVariable lhs || _doesThisNodeContainsVariable rhs

    let rec private _transformExpressionTreeRec (tree : ExpressionNode, reversed : ExpressionNode) =
        match tree with
        | ExpressionNode.Value value  -> reversed
        | ExpressionNode.Variable var -> reversed
        | ExpressionNode.Node (op, lhs, rhs) -> 
            let revOp = _inverseOperation op
            match _doesThisNodeContainsVariable lhs with
            | false ->
                match op with
                | Add      | Multiply -> 
                    let reversedUpdate = ExpressionNode.Node (revOp, reversed, lhs)
                    _transformExpressionTreeRec(rhs, reversedUpdate)
                | Subtract  | Divide -> 
                    let reversedUpdate = ExpressionNode.Node (op, lhs, reversed)
                    _transformExpressionTreeRec(rhs, reversedUpdate) 
            | true -> 
                let reversedUpdate = ExpressionNode.Node (revOp, reversed, rhs)
                _transformExpressionTreeRec(lhs, reversedUpdate)

    let TransformExpressionTree (tree : ExpressionNode) =
        match tree with
        | ExpressionNode.Node (op, lhs, rhs) -> 
            ExpressionNode.Node (op, ExpressionNode.Variable "x", _transformExpressionTreeRec(lhs, rhs))

module EquationEvaluation =  
    open EquationSolverTypes

    let rec private _evaluateTreeRec (tree : ExpressionNode) =
        match tree with
        | ExpressionNode.Value value  -> value
        | ExpressionNode.Variable var -> failwith "There should'nt be any varible while evaluating"
        | ExpressionNode.Node (op, lhs, rhs) -> 
            match op with 
            | Add      -> _evaluateTreeRec(lhs) + _evaluateTreeRec(rhs)
            | Subtract -> _evaluateTreeRec(lhs) - _evaluateTreeRec(rhs)
            | Multiply -> _evaluateTreeRec(lhs) * _evaluateTreeRec(rhs)
            | Divide   -> _evaluateTreeRec(lhs) / _evaluateTreeRec(rhs)

    let EvaluateTree (tree : ExpressionNode) =
        match tree with
        | ExpressionNode.Node (op, lhs, rhs) -> 
            _evaluateTreeRec(rhs)
