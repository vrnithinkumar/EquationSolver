open EquationSolver

// Testing 1+(x*10)=21
let test1() =
    let jsonValue = """ 
        {
            "op": "equal",
            "lhs": {
                "op": "add",
                "lhs": 1,
                "rhs": {
                    "op": "multiply",
                    "lhs": "x",
                    "rhs": 10
                    }
                },
            "rhs": 21
        }
        """
    let value = EquationSolver.ParseFromStringThenEvaluate(jsonValue)
    printfn "JSON : %s \n Evaluated : %M " jsonValue value

// Testing x=42 
let test2() =
    let jsonValue = """ 
        {
            "op": "equal",
            "lhs": "x",
            "rhs": 42
        }
        """
    let value = EquationSolver.ParseFromStringThenEvaluate(jsonValue)
    printfn "JSON : %s \n Evaluated : %M " jsonValue value

// Testing (15-5)+(x*10)=42
let test3() =
    let jsonValue = """ 
        {
            "op": "equal",
            "lhs": {
                    "op": "add",
                    "lhs": {
                    "op": "subtract",
                    "lhs": 15,
                    "rhs": 5
                    },
                "rhs": {
                    "op": "multiply",
                    "lhs": "x",
                    "rhs": 10
                    }
                },
            "rhs": 42
        }
        """
    let value = EquationSolver.ParseFromStringThenEvaluate(jsonValue)
    printfn "JSON : %s \n Evaluated : %M " jsonValue value

// Testing 10/x=5
let test4() =
    let jsonValue = """ 
        {
            "op": "equal",
             "lhs": {
                    "op": "divide",
                    "lhs": 10,
                    "rhs": "x"
                    },
            "rhs": 5
        }
        """
    let value = EquationSolver.ParseFromStringThenEvaluate(jsonValue)
    printfn "JSON : %s \n Evaluated : %M " jsonValue value

// Testing 20/(10-x)=4
let test5() =
    let jsonValue = """ 
        {
            "op": "equal",
            "lhs": {
                    "op": "divide",
                    "lhs": 20,
                    "rhs" :{
                        "op": "subtract",
                        "lhs": 10,
                        "rhs":"x"
                    }
                },
            "rhs": 4
        }
        """
    let value = EquationSolver.ParseFromStringThenEvaluate(jsonValue)
    printfn "JSON : %s \n Evaluated : %M " jsonValue value

[<EntryPoint>]
let main argv = 
    test1()
    test2()
    test3()
    test4()
    test5()
    0