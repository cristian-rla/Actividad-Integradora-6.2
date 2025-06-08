-module(central).
-export([abre_central/1, cierra_central/0, lista_taxis/0, viajes_completados/0, viajes_activos/0,lista_viajeros/0, central/1, viaje/4]).

% Ligar todos los procesos al hijo. LINK con flag.
% Para nodos, monitor node
% Tener un parámetro en la de lista_taxis() para ver si es solo retornar o imprimir
% Si un nodo muere, tomar acción
% si un proceso viajero muere, cambiar el estado de taxi?
% Falta generar un id para el viaje
% Hasta que se borren quitarlos de la lista? 
% Falta el caso en el que el taxista se borre cuando iba por un viajero, hay que asignarle otro taxista

server_central()->'nodoCentral@LAPTOP-ULGC39GQ'.
abre_central({X,Y})->
    case whereis(central) of
        undefined ->
            register(central, spawn(?MODULE, central, [{{X,Y}, [], 0, [],[]}])),
            io:format("Se ha creado la central en la localización {~p,~p}~n", [X,Y]);
        _ ->
            io:format("Ya existe una central~n")
    end
.

cierra_central()->
    {central, server_central()} ! cierra_central.

lista_taxis()->
    {central, server_central()} ! {lista_taxis, self()},
    receive 
        {lista_otorgada, Taxis} -> 
            io:format("Taxis registrados =================================~n"),
            lists:foreach(fun({TaxiId, TaxiPid})-> io:format("->ID:~p PID:~p ~n",[TaxiId, TaxiPid]) end, Taxis)
    after 2000 ->
        timeout
    end
.

lista_viajeros()->
    {central, server_central()} ! {lista_viajeros, self()},
    receive 
        {lista_otorgada, Viajeros} -> 
            io:format("Viajeros registrados =================================~n"),
            lists:foreach(fun({ViajeroId, ViajeroPid})-> io:format("->ID:~p PID:~p ~n",[ViajeroId, ViajeroPid]) end, Viajeros)
    after 2000 ->
        timeout
    end
.

to_string(Term) ->
    case Term of
        _ when is_tuple(Term) ->
            lists:flatten(io_lib:format("~p", [Term]));
        _ when is_atom(Term) ->
            atom_to_list(Term)
    end.

viajes_completados()->
    {central, server_central()} ! {lista_viajes, self()},
    receive 
        {lista_otorgada, List} -> 
            io:format("Viajes Completados ========================================"),
            lists:foreach(fun({ViajeId, TaxiId, NombreViajero, Origen, Destino})-> % tomar los datos de la tabla de pid y taxista para el nombre del taxi
                io:format("->Viaje: ~p Taxista: ~p, Viajero: ~p, Origen: ~p, Destino: ~p~n",[ViajeId, TaxiId, NombreViajero, to_string(Origen), to_string(Destino)])
            end ,List)
    after 2000 ->
        timeout
    end
.

viajes_activos()->
    {central, server_central()} ! {lista_viajes_activos, self()},
    receive 
        {lista_otorgada, List} -> 
            io:format("Viajes Activos ========================================"),
            lists:foreach(fun({ViajeId, TaxiId, {NombreViajero, _}, Origen, Destino})-> % tomar los datos de la tabla de pid y taxista para el nombre del taxi
               io:format("->Viaje: ~p Taxista: ~p, Viajero: ~p, Origen: ~p, Destino: ~p ====================~n",[ViajeId, TaxiId, NombreViajero, to_string(Origen), to_string(Destino)])
            end ,List)
    after 2000 ->
        timeout
    end
.

seleccionaTaxi(aeropuerto, Taxis, ViajesActivos, {Xa, Ya})-> % Falta considerar el caso en el que se pone un átomo. Esta función checa qué taxi disponible es el más cercano
    case Taxis of
        []-> ningun_taxi;
        _ ->
            lists:foldl(
                fun({TaxiId, TaxiPid}, {PasadoTaxi, Distancia}) ->  
                    case lists:keyfind(TaxiId, 2, ViajesActivos) of 
                        false->
                            TaxiPid ! {consulta_estado, self()},
                            receive 
                                {estado, _, {X1, Y1}} -> % Falta considerar el caso en el que se pone un átomo
                                    ActualDistancia = math:sqrt(math:pow(X1-Xa, 2) + math:pow(Y1-Ya, 2)),
                                    case PasadoTaxi of
                                        no_disponible ->
                                            {{TaxiId, TaxiPid}, ActualDistancia};
                                        _ ->
                                            case ActualDistancia < Distancia of
                                                true ->
                                                    {{TaxiId, TaxiPid}, ActualDistancia};
                                                false->
                                                    {PasadoTaxi, Distancia}
                                            end
                                    end;
                                {estado, _, aeropuerto} ->
                                    ActualDistancia = 0,
                                    case PasadoTaxi of
                                        no_disponible ->
                                            {{TaxiId, TaxiPid}, ActualDistancia};
                                        _ ->
                                            case ActualDistancia < Distancia of
                                                true ->
                                                    {{TaxiId, TaxiPid}, ActualDistancia};
                                                false->
                                                    {PasadoTaxi, Distancia}
                                            end
                                    end
                            after 100->
                                {PasadoTaxi, Distancia}
                            end;
                        _ ->
                            {PasadoTaxi, Distancia}
                        end
                end, 
            {no_disponible, infinity}, Taxis)
        end
    ;

seleccionaTaxi({X, Y}, Taxis, ViajesActivos, {Xa, Ya})-> % Falta considerar el caso en el que se pone un átomo. Esta función checa qué taxi disponible es el más cercano
    case Taxis of
        []-> ningun_taxi;
        _ ->
            lists:foldl(
                fun({TaxiId, TaxiPid}, {PasadoTaxi, Distancia}) ->  
                    case lists:keyfind(TaxiId, 2, ViajesActivos) of 
                        false->
                            TaxiPid ! {consulta_estado, self()},
                            receive 
                                {estado, _, {X1, Y1}} -> % Falta considerar el caso en el que se pone un átomo
                                    ActualDistancia = math:sqrt(math:pow(X1-X, 2) + math:pow(Y1-Y, 2)),
                                    case PasadoTaxi of
                                        no_disponible ->
                                            {{TaxiId, TaxiPid}, ActualDistancia};
                                        _ ->
                                            case ActualDistancia < Distancia of
                                                true ->
                                                    {{TaxiId, TaxiPid}, ActualDistancia};
                                                false->
                                                    {PasadoTaxi, Distancia}
                                            end
                                    end;
                                {estado, _, aeropuerto} ->
                                    ActualDistancia = math:sqrt(math:pow(Xa-X, 2) + math:pow(Ya-Y, 2)),
                                    case PasadoTaxi of
                                        no_disponible ->
                                            {{TaxiId, TaxiPid}, ActualDistancia};
                                        _ ->
                                            case ActualDistancia < Distancia of
                                                true ->
                                                    {{TaxiId, TaxiPid}, ActualDistancia};
                                                false->
                                                    {PasadoTaxi, Distancia}
                                            end
                                    end
                            after 100->
                                {PasadoTaxi, Distancia}
                            end;
                        _ ->
                            {PasadoTaxi, Distancia}
                        end
                end, 
            {no_disponible, infinity}, Taxis)
        end
    .

encuentra_taxi(TaxiId, Taxis)->
    case lists:keyfind(TaxiId, 1, Taxis) of
        false->
            no_encontrado;
        {_, TaxiPidEncontrado} ->
            TaxiPidEncontrado
    end
.

central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos})->
    receive
        cierra_central ->
            exit(normal) % Falta terminar los procesos hijos
    after 0 ->
        receive
            {registra_taxi, NombreTaxi, Pid} ->
                io:format("El taxi ~p ha sido correctamente registrado en la central ~n", [NombreTaxi]),
                central({Ubicacion, [{NombreTaxi, Pid} | Taxis], NumViajes, ViajesCompletados, ViajesActivos});

            {elimina_taxi, {TaxiId, TaxiPid}} ->
                io:format("Se ha solicitado la eliminación del taxi ~p~n", [TaxiId]),
                TaxiPid ! terminar_taxi;

            {otorga_taxi, {NombreViajero, PidViajero}, Origen, Destino} ->
                io:format("Se ha recibido una solicitud del viajero ~p: ", [NombreViajero]),
                case seleccionaTaxi(Origen, Taxis, ViajesActivos, Ubicacion) of
                    ningun_taxi->
                        io:format("No hay ningún taxi registrado, ~p notificado~n", [NombreViajero]),
                        PidViajero ! {elimina_viajero, no_taxis},
                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

                    {no_disponible, _} -> 
                        io:format("No hay taxis disponibles, ~p notificado ~n", [NombreViajero]),
                        PidViajero ! {elimina_viajero, taxis_ocupados},
                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

                    {{TaxiId, TaxiPid},_} ->
                        io:format("El taxi ~p está disponible ~n", [TaxiId]),

                        NuevoViaje = spawn(?MODULE, viaje, [Origen, Destino, NombreViajero, TaxiPid]),
                        PidViajero ! {taxi_asignado, NuevoViaje, TaxiId},
                        TaxiPid ! {viajero_asignado, NuevoViaje},
                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, [{NuevoViaje, TaxiId, {NombreViajero, PidViajero}, Origen, Destino} | ViajesActivos] });

                    _ ->
                        io:format("Algo inesperado sucedió ~n"),
                        PidViajero ! {elimina_viajero, no_taxis},
                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos})
                end;

            {servicio_terminado, {TaxiId, TaxiPid}} -> % Traducir al viaje
                case lists:dropwhile(fun({_, ActTaxiId, _, _, _}) -> TaxiId =/= ActTaxiId end, ViajesActivos) of
                    [{IdViaje, _, {NombreViajero, PidViajero}, Origen, Destino}|_] ->
                        io:format("El viaje ~p del taxi ~p ha culminado~n", [IdViaje, TaxiId]),
                        TaxiPid ! {viaje_terminado, Destino},
                        PidViajero ! viaje_terminado,
                        central({Ubicacion, Taxis, NumViajes + 1, [{IdViaje, TaxiId, {NombreViajero, PidViajero}, Origen, Destino} | ViajesCompletados], lists:filter(fun({ActViajeId, _, _, _, _})-> IdViaje =/= ActViajeId end, ViajesActivos)});
                    _ ->
                        io:format("No se encontró el viaje que se quería finalizar~n")
                end,
                central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});


            {cancela_solicitud, From, ViajeroId} ->
                case lists:dropwhile(fun({_, _, {ViajeroActual, _}, _, _})-> ViajeroId =/= ViajeroActual end, ViajesActivos) of
                    [{IdViaje, TaxiId, {_, ViajeroPid}, _, _}|_] ->
                        case encuentra_taxi(TaxiId, Taxis) of
                            no_encontrado ->
                                io:format("El taxi asociado al viaje no se encuentra~n");
                            TaxiPid->
                                TaxiPid ! {consulta_estado, self()},
                                receive
                                    {estado, ocupado, _} ->
                                        io:format("El viajero ~p no cancelar, ya ha sido recogido~n", [ViajeroId]),
                                        From ! cancelacion_invalida,
                                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

                                    {estado, disponible, _}->
                                        io:format("El viajero ~p ha cancelado el viaje, como no ha sido recogido, será eliminado~n", [ViajeroId]),
                                        ViajeroPid ! {elimina_viajero, cancelacion_exitosa},
                                        TaxiPid ! viaje_cancelado,
                                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, lists:filter(fun({ActViajeId, _, _, _, _})-> IdViaje =/= ActViajeId end, ViajesActivos)})
                                end
                        end;
                    _->
                        io:format("El viajero ~p no pertenece todavía a ningún viaje, se le está asignando taxi~n",[ViajeroId]),
                        central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos})

                end;

            {lista_taxis, From} ->
                From ! {lista_otorgada, Taxis},
                central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

            {lista_viajeros, From} ->
                From ! {lista_otorgada, 
                    lists:map(
                        fun({_, _, Viajero, _, _}) -> 
                            Viajero
                        end, ViajesActivos)
                    },
                central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

            {lista_viajes, From} ->
                From ! {lista_otorgada, ViajesCompletados},
                central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

            {lista_viajes_activos, From} ->
                From ! {lista_otorgada, ViajesActivos},
                central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos});

            cierra_central ->
                io:format("La central se cerró correctamente~n"),
                exit(kill); % Falta lógica para matar los procesos hijos

            _ -> 
                central({Ubicacion, Taxis, NumViajes, ViajesCompletados, ViajesActivos})
        end
    end
.

viaje(Origen, Destino, ViajeroPid, TaxiPid)->
    receive 
        viaje_terminado ->
            ViajeroPid ! viaje_terminado, 
            TaxiPid ! {viaje_terminado, Destino};
        _->
            viaje(Origen, Destino, ViajeroPid, TaxiPid)
    end
.
