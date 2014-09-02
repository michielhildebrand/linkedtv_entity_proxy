:- module(entity_proxy,
	  [entity_props/3,
	   entity_prop/4
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(entity_crawl)).
:- use_module(library(entities)).

:- rdf_meta
	rdf_same(r,r,o).

:- http_handler(cliopatria(entity_proxy), http_entity_proxy, []).

http_entity_proxy(Request) :-
	http_parameters(Request,
			[ url(URLs0,
			      [description('URLs'),
			       zero_or_more
			      ]),
			  urls(URL_String,
			       [description('JSON Array with URLs'),
				optional(true),
				atom
			       ]),
			  lang(Lang,
			       [description('Filter entity properties by langauge'),
				optional(true)
			       ]),
			  basic(Basic,
				[description('When set to true only basic information is returned'),
				 default(false)
				]),
			  callback(CallbackFunction,
				   [description('JSONP callback function'),
				    optional(true)
				   ])
			]),
	 (   nonvar(URL_String)
	 ->  atom_json_term(URL_String, URL_List, []),
	     is_list(URL_List)
	 ;   URL_List = []
	 ),
	 append(URLs0, URL_List, URLs),
	 Options = [lang(Lang),basic(Basic)],
	 entities_props(URLs, URL_Props, Options),
	 (   nonvar(CallbackFunction)   % output jsonp
	 ->  format('Content-type: application/javascript~n~n'),
	     format('~w(',CallbackFunction),
	     json_write(current_output, json(URL_Props)),
	     format(');')
	 ;   format('Access-Control-Allow-Origin: *~n'),
	     format('Access-Control-Allow-Methods: GET, POST~n'),
	     reply_json(json(URL_Props))
	 ).


entities_props([], [], _).
entities_props([URL|URLs], [URL-json(Props)|URLs_Props], Options) :-
	entity_props(URL, Props, Options),
	%group_pairs_by_key(Props0, Props),
	entities_props(URLs, URLs_Props, Options).

entity_props(R, [type=Types|Data], Options) :-
	option(lang(Lang), Options),
	crawl_entity(R, []),
	findall(Type, (rdf_same(R, rdf:type, Class),
		       entity_type(Class, Type)
		      ),
		Types0),
	sort(Types0, Types),
	(   option(basic(true), Options)
	->  basic_props(Props)
	;   all_props(Types, Props)
	),
	entity_prop_data(Props, R, Lang, Data).

basic_props(Props) :-
	findall(p(Key,Ps,Resolve),
		( resource(Key,Ps0,Resolve),
		  rdf_global_term(Ps0, Ps)
		),
		Props).

all_props(Types, Props) :-
	findall(p(Key,Ps,Resolve),
		(member(Type,Types),
		 entity_prop(Type, Key, Ps0, Resolve),
		 rdf_global_term(Ps0, Ps)
		),
		Props0),
	sort(Props0, Props).


entity_prop_data([], _, _, []).
entity_prop_data([p(Key,Ps,Resolve)|T], R, Lang, [Key=Data|Rest]) :-
	prop_data(Ps, R, Lang, Resolve, Data),
	entity_prop_data(T, R, Lang, Rest).

prop_data(Ps, R, Lang, Resolve, PropData) :-
	findall(O, (member(P,Ps), rdf_same(R,P,O)), Os),
	empty_assoc(Assoc),
	select_property_values(Os, Lang, Resolve, Assoc, PropData).

select_property_values([], _, _, _, []).
select_property_values([O|T], Lang, Resolve, Assoc, Rest) :-
	get_assoc(O, Assoc, _),
	!,
	select_property_values(T, Lang, Resolve, Assoc, Rest).
select_property_values([O|T], Lang, Resolve, Assoc0, [Data|Rest]) :-
	object_display_data(O, Lang, Resolve, Data),
	put_assoc_rdf_same(O, Assoc0, true, Assoc),
	select_property_values(T, Lang, Resolve, Assoc, Rest).
select_property_values([_O|T], Lang, Resolve, Assoc0, Rest) :-
	select_property_values(T, Lang, Resolve, Assoc0, Rest).

put_assoc_rdf_same(R, Assoc0, Value, Assoc) :-
	findall(R1,same(R,R1),Rs),
	put_assoc_rdf_same_(Rs, Assoc0, Value, Assoc).

put_assoc_rdf_same_([], Assoc, _, Assoc).
put_assoc_rdf_same_([R|Rs], Assoc0, Value, Assoc) :-
	put_assoc(R, Assoc0, Value, Assoc1),
	put_assoc_rdf_same_(Rs, Assoc1, Value, Assoc).


object_display_data(literal(Lit), _Lang, false, Text) :-
	!,
	literal_text(Lit, Text).
object_display_data(literal(Lit), Lang, true, json([lang=LitLang, value=Text])) :-
	!,
	literal_lang(Lit, LitLang),
	(   var(Lang)
	->  true
	;   LitLang = Lang
	),
	literal_text(Lit, Text).
object_display_data(R, _Lang, false, R) :-
	atom(R),
	!.
object_display_data(R, Lang0, true, json([uri=R,lang=Lang,value=Label])) :-
	atom(R),
	%crawl_entity(R, []),
	(   rdf_display_label(R, Lang0, Label)
	->  Lang = Lang0
	;   rdf_display_label(R, Label),
	    Lang = ''
	).


		 /*******************************
		 *               C		*
		 *******************************/



literal_lang(literal(lang(Lang0, _)), Lang) :- !,
	Lang0 = Lang.
literal_lang(lang(Lang0, _), Lang) :- !,
	Lang0 = Lang.
literal_lang(_, '').


rdf_same(S,P,O) :-
	same(S,S1),
	rdf(S1,P,O).


same(E,E).
same(E1,E2) :-
	findall(E,
		(   rdf(E1,owl:sameAs,E)
		;   rdf(E,owl:sameAs,E1)
		),
		Es0),
	sort(Es0, Es),
	member(E2, Es).

