declare
fun {Leaf Value}
   node(leaf Value)
end

fun {Operator Op Left Right}
   node(operator Op Left Right)
end

% Evaluar
fun {Evaluate Node}
   case Node of
      node(leaf Value) then
         if {IsNumber Value} then
            Node
         else
            Node
         end

   [] node(operator Op LeftArg RightArg) then
         LeftVal = {Evaluate LeftArg}
         RightVal = {Evaluate RightArg}

         case LeftVal of node(leaf LV) then
            if not {IsNumber LV} then
               Node
            end
         else
            raise error('Left argument did not reduce to a value') end
         end

         case RightVal of node(leaf RV) then
            if not {IsNumber RV} then
               Node
            end
         else
            raise error('Right argument did not reduce to a value') end
         end

         Result =
            case Op
            of '+' then LV + RV
            [] '-' then LV - RV
            [] '*' then LV * RV
            [] '/' then LV div RV
            else
               raise error('Unknown operator') end
            end

         % Return a leaf node with the result
         {Leaf Result}

   else
      raise error('Unknown node type') end
   end
end

% Example 1: Reducible expression (5 + 5)
Five = {Leaf 5}
OperatorNode1 = {Operator '+' Five Five}
ReducedGraph1 = {Evaluate OperatorNode1}

case ReducedGraph1 of
   node(leaf Value) then
      {Browse Value}  % Displays 10
else
   _ % Do nothing; expression couldn't be reduced further
end

% Example 2: Non-reducible expression (x + x)
XLeaf = {Leaf x}
OperatorNode2 = {Operator '+' XLeaf XLeaf}
ReducedGraph2 = {Evaluate OperatorNode2}
% Graph remains the same as OperatorNode2

% Output the result
case ReducedGraph2 of
   node(leaf Value) then
      {Browse Value}  % Would only display if Value is a number
else
   {Browse 'Expression could not be reduced further.'}
end
