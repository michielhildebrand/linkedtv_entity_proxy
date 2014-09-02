:- module(entities,
	  [entity_type/2,
	   entity_prop/4
	  ]).

:- use_module(library(semweb/rdf_db)).

%%	entity_type(+Class, -Type).
%
%	Type is the normalized type for an RDF Class.

entity_type(_, resource).

entity_type(C, person) :- rdf_equal(C, 'http://dbpedia.org/ontology/Person').
entity_type(C, person) :- rdf_equal(C, 'http://xmlns.com/foaf/0.1/Person').
entity_type(C, person) :- rdf_equal(C, 'http://dbpedia.org/class/yago/Person100007846').
entity_type(C, artist) :- rdf_equal(C, 'http://dbpedia.org/ontology/Artist').
entity_type(C, artist) :- rdf_equal(C, 'http://dbpedia.org/class/yago/Artist109812338').
entity_type(C, artist) :- rdf_equal(C, 'http://dbpedia.org/class/yago/Painter110391653').

entity_type(C, presenter) :- rdf_equal(C, 'http://dbpedia.org/ontology/Presenter').

entity_type(C, museum) :- rdf_equal(C, 'http://dbpedia.org/ontology/Museum').
entity_type(C, museum) :- rdf_equal(C, 'http://schema.org/Museum').

entity_type(C, place) :- rdf_equal(C, 'http://dbpedia.org/ontology/Place').
entity_type(C, place) :- rdf_equal(C, 'http://schema.org/Place').

entity_type(C, artobject) :- rdf_equal(C, 'http://vocab.getty.edu/aat/300264092').

entity_type(C, film) :- rdf_equal(C, 'http://dbpedia.org/ontology/Film').
entity_type(C, film) :- rdf_equal(C, ' http://schema.org/Movie').

entity_type(C, organisation) :- rdf_equal(C, 'http://dbpedia.org/ontology/Company').
entity_type(C, organisation) :- rdf_equal(C, 'http://dbpedia.org/ontology/Organisation').
entity_type(C, organisation) :- rdf_equal(C, 'http://schema.org/Organization').



%%	entity_prop(+Type, -Name, -PropertyURIs, -RecursivlyResolve)
%
%	Name: atom indicating the key shown in the json file returned
%       PropertyURIs: list of all property URIs that are included under
%    this key
%       RecursivlyResolve: boolean indicating if the objects from the
%  resolved URI need to be resolved themselves as well

entity_prop(resource, label, [rdfs:label], true).
entity_prop(resource, thumb, [foaf:depiction], false).
entity_prop(resource, comment, ['http://dbpedia.org/ontology/abstract',
		   rdfs:comment], true).

entity_prop(person, birthDate,   ['http://dbpedia.org/ontology/birthDate'], false).
entity_prop(person, deathDate,   ['http://dbpedia.org/ontology/deathDate'], false).
entity_prop(person, birthPlace,  ['http://dbpedia.org/ontology/birthPlace'], true).
entity_prop(person, deathPlace,  ['http://dbpedia.org/ontology/deathPlace'], true).
entity_prop(person, nationality, ['http://dbpedia.org/ontology/nationality'], true).
entity_prop(person, profession, ['http://dbpedia.org/ontology/occupation'], true).

% LN extension for people who held a role in their lifetimes (doesn't
% seem to be always indicated by a specific type)
entity_prop(person, predecessor, ['http://dbpedia.org/ontology/predecessor'], true).
entity_prop(person, successor, ['http://dbpedia.org/ontology/successor'], true).

entity_prop(artist, style, ['http://dbpedia.org/ontology/movement'], true).

entity_prop(presenter, activeSince, ['http://dbpedia.org/ontology/activeYearsStartYear'], false).
entity_prop(presenter, knownFor, ['http://dbpedia.org/ontology/knownFor'], true).
entity_prop(presenter, presents, ['http://dbpedia.org/ontology/presenter'], true).
%can we give alternative properties, there are often both English and Dutch, or only one or the other
entity_prop(presenter, guestedIn, ['http://nl.dbpedia.org/property/vasteGasten'], true).

entity_prop(museum, locatedIn, ['http://dbpedia.org/ontology/location'], true).

entity_prop(place, population, ['http://dbpedia.org/ontology/populationTotal'], false).

%this is based on http://www.linkedtv.eu/wiki/index.php/Creating_rich_descriptions_of_cultural_artefacts_out_of_a_TV_program#Vocabulary
entity_prop(artobject, createdIn, ['http://simile.mit.edu/2003/10/ontologies/vraCore3#locationCreationSite'], true).
entity_prop(artobject, createdBy, ['http://purl.org/dc/terms/creator'], true).
entity_prop(artobject, currentlyFoundAt, ['http://www.cidoc-crm.org/rdfs/cidoc-crm#P55F.has_current_location'], true).
entity_prop(artobject, hasStyle, ['http://simile.mit.edu/2003/10/ontologies/vraCore3#stylePeriod'], true).
entity_prop(artobject, madeOf, ['http://www.cidoc-crm.org/rdfs/cidoc-crm#P45F.consists_of'], true).
%time periods and price estimates need to query the value of a property of a value of a property!


% from http://www.linkedtv.eu/wiki/index.php/Annotation_types_in_RBB
entity_prop(film, cinematography, ['http://dbpedia.org/ontology/cinematography'], true).
entity_prop(film, director, ['http://dbpedia.org/ontology/director'], true).
entity_prop(film, musicComposer, ['http://dbpedia.org/ontology/musicComposer'], true).
entity_prop(film, starring, ['http://dbpedia.org/ontology/starring'], true).

entity_prop(organisation(chairman, ['http://dbpedia.org/ontology/chairman'], true).
