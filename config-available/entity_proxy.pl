:- module(conf_entity_proxy, []).

/** <module> Entity proxy to crawl and serve information about entities
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(linkedtv, 'http://data.linkedtv.eu/ontology/').

:- use_module(api(entity_proxy)).
