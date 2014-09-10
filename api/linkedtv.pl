:- module(linkedtv,
	  [entity_cache/2,
	   entity_cache_local/2
	  ]).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(xpath)).
:- use_module(library(media_props)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(entity_crawl)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(api(entity_proxy)).
:- use_module(library(http/http_header)).

:- debug(entity_cache).

:- http_handler(cliopatria(entity_cache), http_entity_cache, []).
:- http_handler(cliopatria(episode), http_episode, []).
% :- http_handler(cliopatria(exp/entities/test), http_exp_entities_test,
% []).

http_entity_cache(Request) :-
	http_parameters(Request,
			[ mediaresource(MR,
			      [description('Id or URI of mediaresource')
			      ]),
			  type(Type, [optional(true)]),
			  attributedTo(AttributedTo, [optional(true)]),
			  lang(Lang, [optional(true)]),
			  force(Force, [boolean, default(false)])
			]),
	(   nonvar(Lang)
	->  atom_json_term(Lang, Lang_List, []),
	    is_list(Lang_List)
	;   true
	),
	include(ground, [type(Type),
			 attributedTo(AttributedTo),
			 lang(Lang_List),
			 force(Force)],
		Options),
	entity_cache(MR, Options),
	reply_html_page([], [h1(MR),h2('crawl finished successfully')]).

%%	entity_cache_local(?MediaResource, +Options)
%
%	Gets the RDF from DBpedia for all entities in the database.

entity_cache_local(MediaResource0, Options) :-
	mediaresource_url(MediaResource0, MediaResource),
	debug(entity_cache, 'Fetch data for ~w', [MediaResource]),
	findall(E, entity_annotation(MediaResource, E, _), Es0),
	sort(Es0, Es),
	disambiguation_urls(Es, URLs0),
	sort(URLs0, URLs),
	length(Es, Entity_Count),
	length(URLs, URL_Count),
	debug(entity_cache, 'Found ~w entities', [Entity_Count]),
	debug(entity_cache, '~w with disambiguation url', [URL_Count]),
	crawl_entities(URLs, Options).

entity_cache(MediaResource0, Options) :-
	mediaresource_id_url(MediaResource0, MediaResource),
	debug(entity_cache, 'Fetch data for ~w', [MediaResource]),
	http_mediaresource_entities(MediaResource, Entities, URLs, Options),
	length(Entities, Entity_Count),
	length(URLs, URL_Count),
	debug(entity_cache, 'Found ~w entities', [Entity_Count]),
	debug(entity_cache, '~w with disambiguation url', [URL_Count]),
	crawl_entities(URLs, Options).

http_mediaresource_entities(MediaResource, Entities, URIs, Options) :-
	option(type(EntityType), Options, 'http://data.linkedtv.eu/ontologies/core#Entity'),
	option(attributedTo(AttributedTo), Options, 'http://data.linkedtv.eu/organization/SV/EditorTool'),
	http_open([ host('data.linkedtv.eu'),
		    path('/API/annotation'),
		    search(['hasTarget.isFragmentOf'=MediaResource,
			    'wasAttributedTo'=AttributedTo,
			    'hasBody.type'=EntityType,
			    '_view'='full',
			    '_pageSize'=1000,
			    '_format'=xml
			   ])
		  ],
		  In,
		  []),
	load_xml(In, XML, []),
	close(In),
	findall(E, xpath(XML, //hasBody/label(text), E), Entities0),
	findall(URI, xpath(XML, //hasBody/locator(text), URI), URIs0),
	%close(XML),
	sort(Entities0, Entities),
	sort(URIs0, URIs).

% http://data.linkedtv.eu/API/annotation?hasTarget.isFragmentOf=http://data.linkedtv.eu/media/8a8187f2-3fc8-cb54-0140-7dd099380002&wasAttributedTo=http%3A%2F%2Fdata.linkedtv.eu%2Forganization%2FSV%2FEditorTool&hasBody.type=http%3a%2f%2fdata.linkedtv.eu%2fontologies%2fcore%23Entity&_view=full&_pageSize=1000&_format=xml
%

		 /*******************************
		 *	      entity props	*
		 *******************************/

disambiguation_urls([], []).
disambiguation_urls([R|Rs], [URI|URIs]) :-
	rdf(R, owl:sameAs, URI),
	!,
	disambiguation_urls(Rs, URIs).
disambiguation_urls([_|Rs], URIs) :-
	disambiguation_urls(Rs, URIs).

entity_annotation(MediaResource, R, Type) :-
	var(MediaResource),
	!,
	rdf(R,rdf:type,'http://data.linkedtv.eu/Entity'),
	entity_type(R, Type).
entity_annotation(MediaResource, R, Type) :-
	rdf(Fragment, ma:isFragmentOf, MediaResource),
	rdf(Annotation, oa:hasTarget, Fragment),
	rdf(Annotation, oa:hasBody, R),
	rdf(R,rdf:type,'http://data.linkedtv.eu/Entity'),
	entity_type(R, Type).

		 /*******************************
		 *		Stats		*
		 *******************************/

video_entity_stats(MR0) :-
	mediaresource_url(MR0, MR),
	findall(Type-E, entity_annotation(MR, E, Type), Pairs),
	pairs_values(Pairs, Es),
	keysort(Pairs, Pairs1),
	group_pairs_by_key(Pairs1, Groups),

	format('totals~n'),
	entity_stats(Es),

	format('~n per type~n'),
	(   member(Type-Type_Es, Groups),
	    format('~n~w~n', [Type]),
	    entity_stats(Type_Es),
	    fail
	;   true
	).


entity_stats(Es) :-
	maplist(entity_label, Es, Es_Labels0),
	sort(Es_Labels0, Es_Labels),

	disambiguation_urls(Es, URLs0),
	sort(URLs0, URLs),

	length(Es, Entity_Count),
	length(Es_Labels, Entity_Label_Count),
	length(URLs, URL_Count),
	URL_Count_Perc is round((URL_Count/Entity_Count)*100),

	format('entities:  ~w~n', [Entity_Count]),
	format('unique entity labels: ~w~n', [Entity_Label_Count]),
	format('entities with sameAs link: ~w (~w%)~n', [URL_Count,URL_Count_Perc]).


entity_label(E, Label) :-
	 rdf(E, rdfs:label, Lit),
	 literal_text(Lit, Label).

entity_type(E, Type) :-
	rdf(E, rdf:type, Type),
	\+ rdf_equal(Type, 'http://data.linkedtv.eu/Entity').


		 /*******************************
		 *	   MediaResource	*
		 *******************************/

http_episode(Request) :-
	http_parameters(Request,
			[ mediaresource(MR0,
			      [description('Id or URI of mediaresource')
			      ]),
			  curated(Curated,
				  [boolean,default(true),
				   description('When true only get curated data')
				  ])			]),
	mediaresource_id_url(MR0, MR),
	%EntityType = 'http://data.linkedtv.eu/ontologies/core#Entity',
	(   Curated
	->  AttributedTo = 'http://data.linkedtv.eu/organization/SV/EditorTool'
	;   AttributedTo = ''
	),
	fetch_program(MR0, Title, Date, Publisher, Duration),
	fetch_chapters(MR, AttributedTo, Chapters),
	fetch_entities(MR, AttributedTo, Entities),
	chapter_entities(Chapters, Entities, ChapterData),
	chapter_list(1, ChapterData, ChapterJSON),
	reply_json(json([title= Title,
			 date=Date,
			 source=Publisher,
			 duration=Duration,
			 chapters=ChapterJSON])).


fetch_program(MR, Title, Date, Publisher, Duration) :-
	atomic_list_concat(['/mediaresource/', MR, '.xml'], Path),
		http_open([ host('api.linkedtv.eu'),
		    path(Path),
		    search(['_format'=xml
			   ])
		  ],
		  In,
		  []),
	load_xml(In, XML, []),
	close(In),
	xpath(XML, //titleName(text), Title),
	xpath(XML, //publisher(text), Publisher),
	Date = '2014-06-02',
	Duration = 1800.


fetch_chapters(MR, AttributedTo, Chapters) :-
	http_open([ host('data.linkedtv.eu'),
		    path('/API/annotation'),
		    search(['hasTarget.isFragmentOf'=MR,
			    'wasAttributedTo'=AttributedTo,
			    'hasBody.type'='Chapter',
			    '_view'='full',
			    '_pageSize'=1000,
			    '_format'=xml
			   ])
		  ],
		  In,
		  []),
	load_xml(In, XML, []),
	close(In),
	Chapter = chapter(Start,End,Label),
	findall(Chapter, chapter_in_xml(XML,Start,End,Label), Chapters0),
	sort(Chapters0, Chapters).

chapter_in_xml(XML, Start, End, Label) :-
	xpath(XML, //items/item, Item),
	%xpath_chk(Item, //hasBody//type/item(text), Type),
	%debug(test, '~w', Type),
	%xpath_chk(Item, //hasBody//type/item(@href), 'http://data.linkedtv.eu/ontologies/core#Chapter'),
	xpath_chk(Item, //hasTarget//temporalStart(number), Start),
	xpath_chk(Item, //hasTarget//temporalEnd(number), End),
	xpath_chk(Item, //hasBody//label(text), Label).


fetch_entities(MR, AttributedTo, Entities) :-
	http_open([ host('data.linkedtv.eu'),
		    path('/API/annotation'),
		    search(['hasTarget.isFragmentOf'=MR,
			    'wasAttributedTo'=AttributedTo,
			    'hasBody.type'='http://data.linkedtv.eu/ontologies/core#Entity',
			    '_view'='full',
			    '_pageSize'=1000,
			    '_format'=xml
			   ])
		  ],
		  In,
		  []),
	load_xml(In, XML, []),
	close(In),
	Entity = entity(Start,End,Label,URI),
	findall(Entity, entity_in_xml(XML,Start,End,Label,URI), Entities0),
	sort(Entities0, Entities).


		 /*******************************
		 *               C		*
		 *******************************/

chapter_list(_, [], []).
chapter_list(N, [C|Cs], [JSON|Rest]) :-
	N1 is N+1,
	chapter_json(N, C, JSON),
	chapter_list(N1, Cs, Rest).

chapter_json(N, chapter(Start,End,Title,Entities),
	     json([id=N,
		   startTime=StartTime,
		   duration=Duration,
		   title=Title,
		   fragments=JSON_Entities
		  ])) :-
	StartTime is Start*1000,
	Duration is (Start+End)*1000,
	maplist(entity_json, Entities, JSON_Entities).

entity_json(entity(Start,End,Label,URI),
	  json([startTime=StartTime,
		duration=Duration,
		title=Label,
		locator=URI
	  ])) :-
	StartTime is Start*1000,
	Duration is (Start+End)*1000.

chapter_entities([], _, []).
chapter_entities([chapter(Start,End,Label)|Cs], Entities, [chapter(Start,End,Label,Chapter_Entities)|CsE]) :-
	entities_in_chapter(Entities, Start, End, Chapter_Entities, Rest),
	chapter_entities(Cs, Rest, CsE).

entities_in_chapter([E|Es], Start, End, [E|Es_C], Rest) :-
	E = entity(E_Start,E_End,_,_),
	E_Start >= Start-20,
	E_End =< End,
	!,
	entities_in_chapter(Es, Start, End, Es_C, Rest).
entities_in_chapter([E|Es], Start, End, Es_C, Rest) :-
	E = entity(E_Start,_E_End,_,_),
	E_Start	< End,
	!,
	entities_in_chapter(Es, Start, End, Es_C, Rest).
entities_in_chapter(Entities, _, _, [], Entities).


entity_in_xml(XML, Start, End, Label, URI) :-
	xpath(XML, //items/item, Item),
	xpath_chk(Item, //hasBody//type/item(@href), 'http://data.linkedtv.eu/ontologies/core#Entity'),
	xpath_chk(Item, //hasTarget//temporalStart(number), Start),
	xpath_chk(Item, //hasTarget//temporalEnd(number), End),
	xpath_chk(Item, //hasBody//label(text), Label),
	disambiguation(Item, URI).

disambiguation(Item, URI) :-
	xpath_chk(Item, //hasBody//locator(text), URI),
	!.
disambiguation(Item, URI) :-
	xpath_chk(Item, //hasBody//sameAs(text), URI),
	!.
disambiguation(_Item, '').

entity_data(URI, Lang, Image, Thumb, Desc) :-
	crawl_entity(URI, []),
	(   entity_prop(thumb, URI, ImageURI, [])
	->  http_link_to_id(http_original, [uri(ImageURI)], Image0),
	    http_link_to_id(http_thumbnail, [uri(ImageURI)], Thumb0),
	    http_absolute_uri(Image0, Image),
	    http_absolute_uri(Thumb0, Thumb)
	;   Image = '',
	    Thumb = ''
	),
	(   entity_prop(comment, URI, json(DescObj), [lang([Lang])])
	->  memberchk(value=Desc, DescObj)
	;   Desc = ''
	).


		 /*******************************
		 *               C		*
		 *******************************/

:- use_module(library(http/http_client)).

:- dynamic
	entity_fetch_cache/4.

http_exp_entities(Request) :-
	http_parameters(Request,
			[ subtitleFile(SubtitleFile,
			      [optional(true),
			       description('Path to local subtitle file')
			      ]),
			  subtitleURL(SubtitleURL,
				      [optional(true),
				       description('URL of subtitle file')
				      ]),
			  startdate(StartDate,
				    []),
			  enddate(EndDate,
				  []),
			  lang(Lang, [default(en)])
			]),
	(   nonvar(SubtitleURL)
	->  http_get(SubtitleURL, SubtitleContent, []),
	    Data = xml('text/xml', SubtitleContent)
	;   nonvar(SubtitleFile)
	->  Data =  file('text/xml', SubtitleFile)
	),
	entity_fetch(Data, StartDate, EndDate, Entities0),
	entity_enrich(Entities0, 0, 8, Lang, Entities),
	Chapters = [json([start=0,
			 end=120,
			 title='Fugitive Edward Snowden applies for asylum in Russia',
			 entities=Entities
			])
		   ],
	format('Access-Control-Allow-Origin: *~n'),
	     format('Access-Control-Allow-Methods: GET, POST~n'),
	reply_json(json([title='20:00 News',
			 date='2013-07-03',
			 source='BBC',
			 duration=1800,
			 chapters=Chapters])).

user:file_search_path(jose, '/ufs/hildebra/Projects/linkedtv/newsync/jose').

http_exp_entities_test(Request) :-
	http_parameters(Request,
			[ file(FileName,
			      [description('Path to local data file')
			      ]),
			  lang(Lang, [default(en)])
			]),
	absolute_file_name(jose(FileName), File),
	open(File, read, Stream),
	json_read(Stream, Entities0),
	close(Stream),
	entity_enrich(Entities0, 0, -1, Lang, Entities),
	Chapters = [json([start=0,
			 end=120,
			 title='Egypt\'s Morsi vows to stay in office',
			  links='/js/newsync/jose/cairo_enrichment.json',
			  category='Breaking news',
			 entities=Entities
			])
		   ],
	format('Access-Control-Allow-Origin: *~n'),
	format('Access-Control-Allow-Methods: GET, POST~n'),
	reply_json(json([title='BBC 20:00 News',
			 date='2013-07-03',
			 source='BBC',
			 duration=1800,
			 chapters=Chapters])).

entity_fetch(Data, StartDate, EndDate, Entities) :-
	entity_fetch_cache(Data, StartDate, EndDate, Entities),
	!.
entity_fetch(Data, StartDate, EndDate, Entities) :-
	/*http_post([ protocol(http),
		    host('linkedtv.eurecom.fr'),
		    path('/entitycontext/api/rankedentities'),
		    search([startdate=StartDate,
			    enddate=EndDate,
			    limit=30
			   ])
		  ],
		  Data,
		  Entities0,
		  []),
	*/
	http_get( 'http://localhost/~michiel/linkedtv/exp/newsync/json/snowden.json',
		  Entities0,
		  []),
	(   Entities0 = [json(_)|_]
	->  assert(entity_fetch_cache(Data, StartDate, EndDate, Entities0)),
	    Entities = Entities0
	;   Entities = []
	).


entity_enrich([], _, _, _, []) :- !.
entity_enrich(_, Max, Max, _, []) :- !.
entity_enrich([json(Data)|T], N, Max, Lang, [JSON|Rest]) :-
	N1 is N+1,
	memberchk(uri=URI, Data),
	JSON = json([ start=Start,
		      end=End,
		      label=Label,
		      image=Image,
		      thumb=Thumb,
		      desc=Desc,
		      wikiURL=URI
		    ]),
	crawl_entity(URI, []),
	rdf_display_label(URI, Label),
	entity_prop(thumb, URI, ImageURI, []),
        http_link_to_id(http_original, [uri(ImageURI)], Image0),
	http_link_to_id(http_thumbnail, [uri(ImageURI)], Thumb0),
	http_absolute_uri(Image0, Image),
	http_absolute_uri(Thumb0, Thumb),
	entity_prop(comment, URI, json(DescObj), [lang([Lang])]),
	memberchk(value=Desc, DescObj),
	!,
	Start is N*16,
	End is Start+16,
	entity_enrich(T, N1, Max, Lang, Rest).
entity_enrich([_|T], N, Max, Lang, JSON) :-
	entity_enrich(T, N, Max, Lang, JSON).
