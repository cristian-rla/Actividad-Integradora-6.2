-module(viajero).
-export([solicitar_taxi/3, viajero/2, cancelar_taxi/1]).

central() ->  % Se le ha de llamar al nodo de central nodoCentral
    {ok, Host} = inet:gethostname(),
    list_to_atom("nodoCentral@" ++ Host).

solicitar_taxi(Viajero, Origen, Destino)->
    case verifica_previo_registro(Viajero) of
        false ->
            io:format("El viajero ~p ha solicitado un taxi~n", [Viajero]),
            {central, central()} ! {otorga_taxi, {Viajero, spawn(?MODULE, viajero, [Viajero, undefined])}, Origen, Destino};  
        true ->
            io:format("El viajero ~p ya está registrado ~n", [Viajero]);
        failed_to_connect -> 
            io:format("No se pudo conectar con la central")
    end
.

verifica_previo_registro(NombreViajero)->
    {central, central()} ! {lista_viajeros, self()},
    receive
        {lista_otorgada, Viajeros}->
            lists:any(fun({ActViajeroId, _}) -> NombreViajero == ActViajeroId end, Viajeros)
    after 5000 ->
        failed_to_connect
    end

.

encuentra_viajero(NombreViajero)->
    {central, central()} ! {lista_viajeros, self()},
    receive 
        {lista_otorgada, Viajeros}->
            lists:keyfind(NombreViajero, 1, Viajeros)    

    end
.

% Asume que sí está registrado el viajero es porque existe en los registros
cancelar_taxi(Viajero)->
    case encuentra_viajero(Viajero) of
        false ->
            io:format("No existe tal viajero~n");
        {NombreViajero, PidViajero} ->
            {central, central()} ! {cancela_solicitud, PidViajero, NombreViajero}
    end
.

viajero(NombreViajero, TaxiAsignado)->
    case TaxiAsignado of 
        undefined ->
            receive   
                {taxi_asignado, IdViaje, IdTaxista} ->
                    io:format("Se ha asignado el taxista ~p al viajero ~p, va en camino a realizar el viaje ~p~n", [IdTaxista, NombreViajero, IdViaje]),
                    viajero(NombreViajero, IdTaxista);
                {elimina_viajero, taxis_ocupados} ->
                    io:format("No hay taxis disponibles para este viaje~n");
                {elimina_viajero, no_taxis} ->
                    io:format("No hay taxis disponibles para este viaje~n")
            after 5000 ->
                io:format("No se recibió respuesta de la central~n")
            end;
        _->
            receive
                {elimina_viajero, cancelacion_exitosa} ->
                    io:format("La cancelación solicitada se ha completado. El viajero ~p será eliminado ~n", [NombreViajero] )
            after 0 ->
                receive 
                    cancelacion_invalida -> 
                        io:format("El viajero ~p ya se encuentra en una solicitud activa~n", [NombreViajero]),
                        viajero(NombreViajero, TaxiAsignado);

                    viaje_terminado ->
                        io:format("El viaje ha terminado")
                end
            end
    end
.
