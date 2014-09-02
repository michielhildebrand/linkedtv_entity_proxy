:- module(conf_entity_proxy, []).

/** <module> Entity proxy to crawl and serve information about entities
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(oa, 'http://www.w3.org/ns/oa#').
:- rdf_register_ns(prov, 'http://www.w3.org/ns/prov#').
:- rdf_register_ns(ma, 'http://www.w3.org/ns/ma-ont#').
:- rdf_register_ns(linkedtv, 'http://data.linkedtv.eu/ontology/').
:- rdf_register_ns(lscom, 'http://vocab.linkeddata.es/lscom/').
:- rdf_register_ns(nerd, 'http://nerd.eurecom.fr/ontology#').
:- rdf_register_ns(nsa, 'http://multimedialab.elis.ugent.be/organon/ontologies/ninsuna#').
:- rdf_register_ns(po, 'http://purl.org/ontology/po/').
:- rdf_register_ns(linkedtv_media, 'http://data.linkedtv.eu/media/').

:- use_module(api(entity_proxy)).
