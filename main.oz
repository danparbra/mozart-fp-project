% Task 1

%% Definición de estructura gráfica del programa como records
declare
%% Leaf nodes
fun {Leaf Value}
   node(leaf Value)
end

%% Application nodes
fun {App Left Right}
   node(app Left Right)
end

%% Representación de primitivo suma
Plus = {Leaf '+'}

% Parámetro como nodo leaf
X = {Leaf x}

% Contenido de la función: x + x
FunctionBody = {App {App Plus X} X}

% Guardar la función 'twice' en un map Functions
Functions = {'Map'.new()}
{Functions.put(twice FunctionBody)}

% Aplicación inicial: twice 5
TwiceFunction = {Leaf twice}
Five = {Leaf 5}
InitialGraph = {App TwiceFunction Five}

% Función Substitution
fun {Substitute Node Env}
   case Node of
      node(leaf Var) then
         if {Env.hasKey Var} then
            {Env.get Var}
         else
            Node
         end
   [] node(app Left Right) then
         node(app {Substitute Left Env} {Substitute Right Env})
   else
      Node
   end
end

Env = {'Map'.new()}
{Env.put(x Five)}
FunctionBody = {Functions.get(twice)}
SubstitutedGraph = {Substitute FunctionBody Env}
