:- module(entities,
	  [entity_type/2,
	   entity_prop/4
	  ]).

:- use_module(library(semweb/rdf_db)).

%%	entity_type(+Class, -Type).
%
%	Type is the normalized type for an RDF Class.

entity_type(_, resource).

entity_type(C, work) :- rdf_equal(C, 'http://dbpedia.org/ontology/Work').

entity_type(C, television) :- rdf_equal(C, 'http://dbpedia.org/ontology/TelevisionShow').

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

entity_type(C, politicalparty) :- rdf_equal(C, 'http://dbpedia.org/ontology/PoliticalParty').

entity_type(C, politician) :- rdf_equal(C, 'http://dbpedia.org/ontology/OfficeHolder').
entity_type(C, politician) :- rdf_equal(C, 'http://umbel.org/umbel/rc/Politician').
entity_type(C, politician) :- rdf_equal(C, 'http://dbpedia.org/class/yago/Officeholder110371450').
entity_type(C, politician) :- rdf_equal(C, 'http://dbpedia.org/class/yago/Politician110451263').


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

%can we give alternative properties, there are often both English and Dutch/German, or only one or the other

% from http://www.linkedtv.eu/wiki/index.php/Annotation_types_in_TKK

entity_prop(television, creator, ['http://dbpedia.org/ontology/creator'], true).
entity_prop(television, genre, ['http://dbpedia.org/ontology/genre'], false).
entity_prop(television, network, ['http://dbpedia.org/ontology/network'], false).
entity_prop(television, numberOfEpisodes, ['http://dbpedia.org/ontology/numberOfEpisodes'], false).
entity_prop(television, numberOfSeasons, ['http://dbpedia.org/ontology/numberOfSeasons'], false).
entity_prop(television, releaseDate, ['http://dbpedia.org/ontology/releaseDate'], false).
entity_prop(television, starring, ['http://dbpedia.org/ontology/starring'], true).


entity_prop(artist, style, ['http://dbpedia.org/ontology/movement'], true).

entity_prop(presenter, activeSince, ['http://dbpedia.org/ontology/activeYearsStartYear'], false).
entity_prop(presenter, knownFor, ['http://dbpedia.org/ontology/knownFor'], true).
entity_prop(presenter, presents, ['http://dbpedia.org/ontology/presenter'], true).
entity_prop(presenter, guestedIn, ['http://nl.dbpedia.org/property/vasteGasten'], true).

entity_prop(museum, locatedIn, ['http://dbpedia.org/ontology/location'], true).

entity_prop(place, population, ['http://dbpedia.org/ontology/populationTotal'], false).
entity_prop(place, latitude, ['http://www.w3.org/2003/01/geo/wgs84_pos#lat'], false).
entity_prop(place, longitude, ['http://www.w3.org/2003/01/geo/wgs84_pos#long'], false).
entity_prop(place, region, ['http://dbpedia.org/property/subdivisionName'], true).

%more specific properties when the place is a building or other construction
entity_prop(place, location, ['http://dbpedia.org/property/location'], true).
entity_prop(place, owner, ['http://de.dbpedia.org/property/eigentümer'], true).
entity_prop(place, openingDate, ['http://de.dbpedia.org/property/eröffnung'], false).
entity_prop(place, architect, ['http://de.dbpedia.org/property/architekt'], true).
entity_prop(place, builtBy, ['http://de.dbpedia.org/property/bauherr','http://de.dbpedia.org/property/baumeister'], true).
entity_prop(place, builtYear, ['http://de.dbpedia.org/property/baujahr'], false).
entity_prop(place, architecture, ['http://de.dbpedia.org/property/baustil'], false).

%more specific properties when the place is an administrative region
entity_prop(place, localLeader, ['http://dbpedia.org/ontology/leaderName'], true).
entity_prop(place, localLeaderTitle, ['http://dbpedia.org/ontology/leaderTitle'], false).
entity_prop(place, localLeaderParty, ['http://de.dbpedia.org/property/partei'], true).

%this is based on http://www.linkedtv.eu/wiki/index.php/Creating_rich_descriptions_of_cultural_artefacts_out_of_a_TV_program#Vocabulary
entity_prop(artobject, createdIn, ['http://simile.mit.edu/2003/10/ontologies/vraCore3#locationCreationSite'], true).
entity_prop(artobject, createdBy, ['http://purl.org/dc/terms/creator'], true).
entity_prop(artobject, currentlyFoundAt, ['http://www.cidoc-crm.org/rdfs/cidoc-crm#P55F.has_current_location'], true).
entity_prop(artobject, hasStyle, ['http://simile.mit.edu/2003/10/ontologies/vraCore3#stylePeriod'], true).
entity_prop(artobject, madeOf, ['http://www.cidoc-crm.org/rdfs/cidoc-crm#P45F.consists_of'], true).
%time periods and price estimates need to access properties along a path, is that possible?


% from http://www.linkedtv.eu/wiki/index.php/Annotation_types_in_RBB
entity_prop(film, cinematography, ['http://dbpedia.org/ontology/cinematography'], true).
entity_prop(film, director, ['http://dbpedia.org/ontology/director'], true).
entity_prop(film, musicComposer, ['http://dbpedia.org/ontology/musicComposer'], true).
entity_prop(film, starring, ['http://dbpedia.org/ontology/starring'], true).

entity_prop(organisation, chairman, ['http://dbpedia.org/ontology/chairman'], true).
entity_prop(organisation, focus, ['http://de.dbpedia.org/property/focus'], true).
entity_prop(organisation, formationYear, ['http://dbpedia.org/ontology/formationYear'], false).
entity_prop(organisation, founder, ['http://dbpedia.org/ontology/foundedBy','http://de.dbpedia.org/property/founder'], true).
entity_prop(organisation, foundingYear, ['http://dbpedia.org/ontology/foundingYear','http://de.dbpedia.org/property/gr%C3%BCndungsdatum'], false).
entity_prop(organisation, industry, ['http://dbpedia.org/ontology/industry'], true).
entity_prop(organisation, location, ['http://dbpedia.org/ontology/location'], true).
entity_prop(organisation, locationCity, ['http://dbpedia.org/ontology/locationCity'], true).
entity_prop(organisation, numberEmployees, ['http://dbpedia.org/ontology/numberOfEmployees'], false).

entity_prop(politicalparty, headquarter, ['http://dbpedia.org/ontology/headquarter'], false).
entity_prop(politicalparty, deputyLeader, ['http://dbpedia.org/ontology/secondLeader'], false).
entity_prop(politicalparty, politicalLeaning, ['http://de.dbpedia.org/property/ausrichtung'], false).
%please check the effect of using special characters in properties
entity_prop(politicalparty, leader, ['http://de.dbpedia.org/property/bundesgeschäftsführer'], false).
entity_prop(politicalparty, euParlament, ['http://de.dbpedia.org/property/euParlament'], false).
entity_prop(politicalparty, founding, ['http://de.dbpedia.org/property/gründung'], false).
entity_prop(politicalparty, foundingLocation, ['http://de.dbpedia.org/property/gründungsort'], true).
entity_prop(politicalparty, chairperson, ['http://de.dbpedia.org/property/parteivorsitzende'], true).

entity_prop(politician, activeSince, ['http://dbpedia.org/ontology/activeYearsStartDate'], false).
entity_prop(politician, activeUntil, ['http://dbpedia.org/ontology/activeYearsEndDate'], false).
entity_prop(politician, office, ['http://dbpedia.org/ontology/office'], false).
entity_prop(politician, party, ['http://dbpedia.org/ontology/party'], true).
entity_prop(politician, officeholderBefore, ['http://dbpedia.org/property/before'], true).
entity_prop(politician, officeholderAfter, ['http://dbpedia.org/property/after'], true).



