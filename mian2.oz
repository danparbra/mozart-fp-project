
local
   fun {MakeNumber Val}
      number(value:Val)
   end

   fun {MakeVariable Name}
      variable(name:Name)
   end

   fun {MakeApplication Func Arg}
      application(func:Func arg:Arg)
   end

   fun {MakePrimitive Name Arity Operation}
      primitive(name:Name arity:Arity op:Operation)
   end

   fun {MakeSupercombinator Name Params Body}
      supercombinator(name:Name params:Params body:Body)
   end

   fun {EnvLookup Env Key}
      case Env
      of nil then false
      [] H|T then
         if H.1 == Key then H.2
         else {EnvLookup T Key}
         end
      end
   end

   fun {EnvExtend Env Key Value}
      (Key#Value)|Env
   end

   Primitives = [
      '+'#primitive(
         name:'+'
         arity:2
         op:fun {$ X Y} {MakeNumber X.value + Y.value} end
      )
      '*'#primitive(
         name:'*'
         arity:2
         op:fun {$ X Y} {MakeNumber X.value * Y.value} end
      )
   ]

   fun {TokenizeExpression Str}
      {String.tokens Str & }
   end

   fun {ParseExpression Tokens}
      case Tokens
      of nil then nil
      [] [Token] then
         if {String.isInt Token} then
            {MakeNumber {String.toInt Token}}
         elsecase {EnvLookup Primitives Token}
         of false then {MakeVariable Token}
         [] Prim then Prim
         end
      [] [Op T2 T3] andthen {EnvLookup Primitives Op} \= false then
         local Left Right in
            Left = {ParseExpression [T2]}
            Right = {ParseExpression [T3]}
            {MakeApplication 
               {MakeApplication {EnvLookup Primitives Op} Left}
               Right}
         end
      else
         local First Rest in
            First = {ParseExpression [Tokens.1]}
            Rest = {ParseExpression Tokens.2}
            {MakeApplication First Rest}
         end
      end
   end

   fun {ParseFunction Line}
      local Parts EqPos Name Params Body in
         Parts = {TokenizeExpression Line}
         if Parts.1 \= "fun" then
            {Show "Error: Function definition must start with 'fun'"}
            nil
         else
            Name = Parts.2
            % Find position of equals sign
            EqPos = {List.takeWhile Parts fun {$ X} X \= "=" end}
            Params = {List.drop {List.take Parts EqPos.length} 2}
            Body = {ParseExpression {List.drop Parts EqPos.length+1}}
            {MakeSupercombinator Name Params Body}
         end
      end
   end

   fun {FindRedexHelper Node Depth}
      case Node
      of application(func:F arg:_) then
         case F
         of application(func:_ arg:_) then
            {FindRedexHelper F Depth+1}
         [] supercombinator(...) then
            if Depth >= {Length F.params}-1 then Node
            else false
            end
         [] primitive(...) then
            if Depth >= F.arity-1 then Node
            else false
            end
         else false
         end
      else false
      end
   end

   fun {FindRedex Node}
      {FindRedexHelper Node 0}
   end

   fun {Reduce Node Env Functions}
      case Node
      of number(...) then Node
      [] variable(name:Name) then
         case {EnvLookup Env Name}
         of false then Node
         [] Value then Value
         end
      [] primitive(...) then Node
      [] supercombinator(...) then Node
      [] application(func:F arg:A) then
         local Redex in
            Redex = {FindRedex Node}
            case Redex
            of false then
               {MakeApplication 
                  {Reduce F Env Functions}
                  {Reduce A Env Functions}}
            else
               case Redex.func
               of primitive(name:_ arity:_ op:Op) then
                  local Arg1 Arg2 in
                     Arg1 = {Reduce Redex.arg Env Functions}
                     Arg2 = {Reduce A Env Functions}
                     case Arg1#Arg2
                     of number(...)#number(...) then
                        {Op Arg1 Arg2}
                     else Node
                     end
                  end
               [] supercombinator(name:Name params:Params body:Body) then
                  local NewEnv in
                     NewEnv = {EnvExtend Env Params.1 Redex.arg}
                     if {Length Params} == 1 then
                        {Reduce Body NewEnv Functions}
                     else
                        {MakeSupercombinator 
                           Name 
                           Params.2 
                           {Reduce Body NewEnv Functions}}
                     end
                  end
               else Node
               end
            end
         end
      end
   end

   fun {ProcessLine Line Functions}
      if {String.isPrefix "fun " Line} then
         local NewFunc in
            NewFunc = {ParseFunction Line}
            if NewFunc == nil then Functions
            else {EnvExtend Functions NewFunc.name NewFunc}
            end
         end
      else
         Functions
      end
   end

   fun {EvaluateProgram Lines Functions LastExpr}
      case Lines
      of nil then
         if LastExpr == nil then nil
         else {Reduce LastExpr nil Functions}
         end
      [] Line|Rest then
         local NewFunctions NewExpr in
            NewFunctions = {ProcessLine Line Functions}
            if {String.isPrefix "fun " Line} then
               NewExpr = LastExpr
            else
               NewExpr = {ParseExpression {TokenizeExpression Line}}
            end
            {EvaluateProgram Rest NewFunctions NewExpr}
         end
      end
   end

   fun {Evaluate Program}
      {EvaluateProgram Program nil nil}
   end
in
   local TestProgram1 TestProgram2 TestProgram3 in
      TestProgram1 = [
         "fun twice x = x + x"
         "twice 5"
      ]
      {Show "Testing twice function:"}
      {Show {Evaluate TestProgram1}}

      TestProgram2 = [
         "fun square x = x * x"
         "square square 3"
      ]
      {Show "Testing square function:"}
      {Show {Evaluate TestProgram2}}

      TestProgram3 = [
         "fun double x = x + x"
         "fun quadruple x = double double x"
         "quadruple 3"
      ]
      {Show "Testing nested function calls:"}
      {Show {Evaluate TestProgram3}}
   end
end
