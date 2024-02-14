import Solve (solve_1, solve_2)
import System.IO
import System.Environment
import System.Exit

-- ZPRACOVANI ARGUMENTU, CHYBOVE VYPISY

main = do

    let help_msg = " Use --help to get more information."
    let use_information = "Basic usage './Main argument [file]', argument = '-i' | '-b' | '-o', file = path containing knapsack problem. For more details open README. "
    let format_input = ["Knapsack {", "maxWeight:", "minCost:", "items: ["]

    args <- getArgs
    case args of
        []      -> die $ "Too few arguments!" ++ help_msg
        (x:[])  -> if x == "--help" then putStrLn use_information else (if x == "-i" || x == "-b" || x == "-o" then solve_1 x format_input else die $ "Wrong arguments!" ++ help_msg)
        (x:y:[]) -> if x == "-i" || x == "-b" || x == "-o" then solve_2 x y format_input else die $ "Wrong arguments!" ++ help_msg 
        (x:y:z:_) -> die $ "Too many arguments!" ++ help_msg
