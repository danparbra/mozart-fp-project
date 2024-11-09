declare
fun {IsNumber X}
   {Number.is X}  % Checks if X is a number
end

fun {Leaf Value}
   node(leaf Value)
end

fun {Operator Op Left Right}
   node(operator Op Left Right)
end

% Evaluation function
fun {Evaluate Node}
   case Node of
      node(leaf Value) then
         % Check if it's a number or a variable
      if {IsNumber Value} then
            % It's a numeric value
         Node
      else
            % It's a variable; cannot reduce further
         Node
      end

   [] node(operator Op LeftArg RightArg) then
         % Evaluate the left and right arguments
      LeftVal = {Evaluate LeftArg}
      RightVal = {Evaluate RightArg}
   in
      case LeftVal of
         node(leaf LV) then
         case RightVal of
            node(leaf RV) then
                     % Check if both arguments are numeric values
            if {IsNumber LV} andthen {IsNumber RV} then
                        % Both arguments are numeric; perform the operation
               Result =
               case Op of
                  '+' then LV + RV
               [] '-' then LV - RV
               [] '*' then LV * RV
               [] '/' then LV div RV
               else
                  raise 'Unknown operator' end
               end
            in
               {Leaf Result}
            else
                        % Cannot reduce further; return the operator node as is
               Node
            end  % End of if

         else
            raise 'Right argument did not reduce to a value' end  % End of inner case
         end  % End of inner case

      else
         raise 'Left argument did not reduce to a value' end  % End of outer case
      end  % End of outer case

   else
      raise 'Unknown node type' end  % End of main case
   end
end

% Example 1: Reducible expression (5 + 5)
Five = {Leaf 5}
OperatorNode1 = {Operator '+' Five Five}
ReducedGraph1 = {Evaluate OperatorNode1}

% Output the result
case ReducedGraph1 of
   node(leaf Value) then
   {Browse Value}  % This will display 10
else
   skip  % Do nothing if not reducible to a value
end

% Example 2: Non-reducible expression (x + x)
XLeaf = {Leaf x}
OperatorNode2 = {Operator '+' XLeaf XLeaf}
ReducedGraph2 = {Evaluate OperatorNode2}

% Output the result
case ReducedGraph2 of
   node(leaf Value) then
   {Browse Value}  % Would display if Value is a number
else
   {Browse 'Expression could not be reduced further.'}
end
