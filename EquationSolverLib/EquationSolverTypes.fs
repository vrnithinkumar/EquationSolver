namespace EquationSolver
module EquationSolverTypes =
    type Operation =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Equal
        | NotSupported

    type ExpressionNode =
        | Node of Operation * ExpressionNode * ExpressionNode
        | Value of decimal 
        | Variable of string
