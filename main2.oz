% Universidad de los Andes
% ISIS4217: Paradigmas de Programación
% David Andres Paredes Bravo - 202315719
% Nicolás Londoño - 201821364

% FP project
% --------------------

declare
% Estructuras base
fun {MakeNumber Value}
   number(value:Value)
end

fun {MakeVariable Name}
   variable(name:Name)
end

fun {MakeApplication Func Arg}
   application(func:Func arg:Arg)
end

fun {MakePrimitive Name Arity Op}
   primitive(name:Name arity:Arity op:Op)
end

fun {MakeSupercombinator Name Params Body}
   supercombinator(name:Name params:Params body:Body)
end

% Ambiente
fun {EnvLookup Env Name}
   case Env
   of nil then false
   [] H|T then
      if H.1 == Name then H.2
      else {EnvLookup T Name}
      end
   end
end

fun {EnvExtend Env Name Value}
   (Name#Value)|Env
end

% Operaciones primitivas a soportar (+, -, *, /)
Primitives = [
   '+'#primitive(
      name:'+'
      arity:2
      op:fun {$ X Y} {MakeNumber X.value + Y.value} end
   )
   '-'#primitive(
      name:'-'
      arity:2
      op:fun {$ X Y} {MakeNumber X.value - Y.value} end
   )
   '*'#primitive(
      name:'*'
      arity:2
      op:fun {$ X Y} {MakeNumber X.value * Y.value} end
   )
   '/'#primitive(
      name:'/'
      arity:2
      op:fun {$ X Y} {MakeNumber {Int.toFloat X.value} / {Int.toFloat Y.value}} end
   )
   'mod'#primitive(
      name:'mod'
      arity:2
      op:fun {$ X Y} {MakeNumber X.value mod Y.value} end
   )
]

% Parseador
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
      Parts = {String.tokens Line & }
      if Parts.1 \= "fun" then
         nil
      else
         Name = Parts.2
         EqPos = {List.takeWhile Parts fun {$ X} X \= "=" end}
         Params = {List.drop {List.take Parts EqPos.length} 2}
         Body = {ParseExpression {List.drop Parts EqPos.length+1}}
         {MakeSupercombinator Name Params Body}
      end
   end
end

% Reducción
fun {FindRedex Node}
   fun {FindRedexRec Node Depth}
      case Node
      of application(func:F arg:_) then
         case F
         of application(func:_ arg:_) then
            {FindRedexRec F Depth+1}
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
in
   {FindRedexRec Node 0}
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
                     {MakeSupercombinator Name Params.2
                        {Reduce Body NewEnv Functions}}
                  end
               end
            else Node
            end
         end
      end
   end
end

fun {IsPrefix S1 S2}
    case S1#S2
    of nil#_ then true
    [] (_|_)#nil then false
    [] (H1|T1)#(H2|T2) then
        if H1 == H2 then {IsPrefix T1 T2}
        else false
        end
    end
end

% Evaluador
fun {Evaluate Program}
   local
      % Función anidada de ayuda para separar progama por cada línea
      fun {SplitIntoLines Str}
         {String.tokens Str &\n}
      end

      % Función anidada de ayuda para convertir resultado a string
      fun {ResultToString Result}
         case Result
         of nil then "nil"
         [] number(value:V) then {VirtualString.toString V}
         [] variable(name:N) then N
         [] primitive(name:N ...) then N
         [] supercombinator(name:N ...) then N
         [] application(func:F arg:A) then
            "(" # {ResultToString F} # " " # {ResultToString A} # ")"
         else "unknown result"
         end
      end

      % Función anidada de ayuda para evaluar cada línea
      fun {EvaluateLines Lines Functions LastExpr}
         case Lines
         of nil then
            if LastExpr == nil then nil
            else {Reduce LastExpr nil Functions}
            end
         [] Line|Rest then
            % Convert Line to string if it isn't already
            local StrLine = {VirtualString.toString Line} in
               if {IsPrefix "fun " StrLine} then
                  local NewFunc NewFunctions in
                     NewFunc = {ParseFunction StrLine}
                     if NewFunc == nil then
                        {EvaluateLines Rest Functions LastExpr}
                     else
                        NewFunctions = {EnvExtend Functions NewFunc.name NewFunc}
                        {EvaluateLines Rest NewFunctions LastExpr}
                     end
                  end
               else
                  local NewExpr in
                     NewExpr = {ParseExpression {String.tokens StrLine & }}
                     {EvaluateLines Rest Functions NewExpr}
                  end
               end
            end
         end
      end

      Lines = if {IsRecord Program} andthen {Label Program} == '|' then Program
              else {SplitIntoLines Program}
              end
      Result = {EvaluateLines Lines nil nil}
   in
      {ResultToString Result}
   end
end

% Test
declare
Program = "
fun twice x = x + x
twice 5
"
declare
Result = {Evaluate Program}
{Browse Result}  % Tiene que mostrar "10"

declare
Program2 = "
5 + 5
"
declare
Result2 = {Evaluate Program2}
{Browse Result2}  % Tiene que mostrar "10"
