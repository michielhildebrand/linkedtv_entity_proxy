:- module(entity_crawl,
	  [crawl_entities/2,
	   crawl_entity/2,
	   recrawl_entities/0
	  ]).

:- use_module(library(xpath)).
:- use_module(library(http/http_client)).
:- use_module(api(lod_crawler)).
:- use_module(library(thread)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(api(entity_proxy)).

:- setting(sameas_load, boolean, true,
	   'Load Same As URLs as well').

:- debug(entity_crawl).

%%	crawl_entities(+EntityURLs, +Options)
%
%	Resolve EntityURLs and store RDF data

crawl_entities([], _).
crawl_entities([IRI|IRIs], Options) :-
	crawl_entity(IRI, Options),
	crawl_entities(IRIs, Options).

crawl_entity(IRI, Options) :-
	option(force(Force), Options, false),
	option(lang(Langs), Options, [en,nl,de,fr]),
	uri_iri(URL,IRI),
	debug(entity_crawl, 'crawl ~w', [IRI]),
	(   Force = false,
	    rdf(URL, linkedtv:crawl_date, DateLit),
	    literal_text(DateLit, Date)
	->  debug(entity_crawl, 'already crawled at: ~w', [Date])
	;   get_time(Now),
	    rdf_assert(URL, linkedtv:crawl_date, literal(Now), crawl),
	    (   dbpedia_uri(URL)
	    ->  load_lod(URL, Force),
		load_lod_sameas(URL, Force, Langs)
	    ;   wikipedia_uri(URL)
	    ->  debug(entity_crawl, '~w fetch WikiPedia page', [URL]),
		load_wiki(URL, Force),
		load_wiki_sameas(URL, Force, Langs)
	    ;   debug(entity_crawl, 'unknown disambiguation URL: ~w', [URL])
	    )
	).

recrawl_entities :-
	forall(rdf(URI, linkedtv:crawl_date, _),
	       (   rdf_retractall(URI, linkedtv:crawl_date, _),
		   crawl_entity(URI, [force(true)])
	       )).

dbpedia_uri(URI) :-
	sub_atom(URI, _, _, _, 'dbpedia.org'),
	!.

wikipedia_uri(URI) :-
	sub_atom(URI, _, _, _, 'wikipedia.org'),
	!.

load_wiki(URL, Force) :-
	(   Force = false,
	    rdf(URL, _, _, URL)
	->  true
	;   http_get(URL, HTML, [])
	->  debug(entity_crawl, 'fetched wiki page from ~w', [URL]),
	    assert_wiki_props(URL, HTML)
	).

assert_wiki_props(URL, HTML) :-
	wiki_lang(URL, Lang),
	(   wiki_summary(HTML, P)
	->  rdf_assert(URL, rdfs:comment, literal(lang(Lang, P)), URL),
	    debug(entity_crawl, '~w stored wiki summary', [URL])
	;   debug(entity_crawl, '~w no wiki summary found', [URL])
	),
	(   wiki_thumb(HTML, Image)
	->  rdf_assert(URL, foaf:depiction, Image, URL),
	    debug(entity_crawl, '~w stored wiki image', [URL])
	;   debug(entity_crawl, '~w no wiki image found', [URL])
	),
	(   wiki_language_links(HTML, Links),
	    \+ Links = []
	->  forall(member(Same, Links),
		   rdf_assert(URL, owl:sameAs, Same, URL)
		  ),
	    length(Links, LinkCount),
	    debug(entity_crawl, '~w stored ~w inter-wiki links', [URL, LinkCount])
	;   debug(entity_crawl, '~w no language links', [URL])
	).

wiki_summary(HTML, P1) :-
	xpath(HTML, //div(@id=bodycontent)//p(1,text), P1),
	!.
wiki_summary(HTML, P1) :-
	xpath(HTML, //div(@id=bodyContent)//p(1,text), P1),
	!.

wiki_thumb(HTML, Thumbnail) :-
	%xpath(HTML, //div(@id=bodycontent), Body),
	(   xpath(HTML, //table//a(@class=image,1)/img(@src), Thumbnail)
	->  true
	;   xpath(HTML, //a(@class=image,1)/img(@class=thumbimage, @src), Thumbnail)
	->  true
	%;   xpath(HTML, //a(@class=image,1)/img(@src), Thumb_URI)
	).

wiki_language_links(HTML, Links) :-
	findall(Link, (xpath(HTML, //div(@id='p-lang')//li/a(@href), Link0),
		       atomic_concat('http:', Link0, Link)
		      ),
		Links).

load_wiki_sameas(URI, Force, Langs) :-
	findall(load_wiki(Same, Force),
		(   rdf(URI, owl:sameAs, Same),
		    \+ Same = URI,
		    sameas_lang_filter(Same, Langs)
		), Cmds0),
	sort(Cmds0, Cmds),
	length(Cmds, Count),
	debug(entity_crawl, '~w fetch ~w sameAs resources', [URI, Count]),
	concurrent(5, Cmds, []).

wiki_lang(URL, Lang) :-
	sub_atom(URL, 7, 2, _, Lang),
	!.
wiki_lang(_URL, en).


load_lod(URI, Force) :-
	lod_uri_graph(URI, Graph),
	(   Force = false,
	    rdf_graph(Graph)
	->  debug(entity_crawl, '~w graph exists already', [URI])
	;   debug(entity_crawl, '~w fetch rdf', [URI]),
	    catch(rdf_load(Graph,
			   [ graph(Graph)
			   ]), _, true)
	).


load_lod_sameas(URI, Force, Langs) :-
	setting(sameas_load, true),
	findall(load_lod(Same, Force),
		(   same(URI, Same),
		    \+ Same = URI,
		    sameas_lang_filter(Same, Langs)
		), Cmds0),
	sort(Cmds0, Cmds),
	length(Cmds, Count),
	debug(entity_crawl, '~w fetch ~w sameAs resources', [URI, Count]),
	concurrent(5, Cmds, []).

sameas_lang_filter(URI, _Languages) :-
	sub_atom(URI, 7, 7, _, dbpedia),
	!.
sameas_lang_filter(URI, Languages) :-
	sub_atom(URI, 7, 2, _, Lang),
	memberchk(Lang, Languages).

:- multifile
	rdf_http_plugin:rdf_content_type/2.

rdf_http_plugin:rdf_content_type('text/xml', xml).
rdf_http_plugin:rdf_content_type('application/xml', xml).

