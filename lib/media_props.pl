:- module(media_props,
	  [episode_title/2,
	   episode_description/2,
	   mediaresource_url/2,
	   mediaresource_id_url/2,
	   mediaresource_of/2,
	   mediaresource_video_source/2,
	   mediaresource_image_source/2,
	   time_property/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).

:- setting(linkedtv:image_server, atom, 'http://images1.noterik.com/',
	       ['Path to image server']).


mediaresource_url(MediaResourceURL, MediaResourceURL) :-
	rdf(MediaResourceURL, rdf:type, ma:'MediaResource'),
	!.
mediaresource_url(MediaResourceId, MediaResourceURL) :-
	rdf_global_id(linkedtv_media:MediaResourceId, MediaResourceURL),
	rdf(MediaResourceURL, rdf:type, ma:'MediaResource'),
	!.


mediaresource_id_url(MediaResourceURL, MediaResourceURL) :-
	rdf_global_id(linkedtv_media:_,	MediaResourceURL),
	!.
mediaresource_id_url(MediaResourceId, MediaResourceURL) :-
	rdf_global_id(linkedtv_media:MediaResourceId, MediaResourceURL),
	!.


episode_title(R, Label) :-
	rdf_label(R, Lit),
	literal_text(Lit, Label).

episode_description(R, Lit) :-
	rdf(R, po:long_synopsis, Lit).



mediaresource_of(MediaResource, MediaResource) :-
	rdf(MediaResource, rdf:type, ma:'MediaResource'),
	!.
mediaresource_of(Episode, MediaResource) :-
	rdf(Episode, rdf:type, po:'Version'),
	!,
	rdf(Episode, linkedtv:hasMediaResource, MediaResource).
mediaresource_of(Episode, MediaResource) :-
	rdf(Episode, rdf:type, po:'Episode'),
	!,
	rdf(Episode, po:version, Version),
	mediaresource_of(Version, MediaResource).

mediaresource_video_source(MediaResource, URL) :-
	rdf(MediaResource, ma:locator, URL).
	%sub_atom(URL, _, _, 0, 'mp4'). % we only use mp4 for know

mediaresource_image_source(MediaResource, URL) :-
	 setting(linkedtv:image_server, Server),
	 mediaresource_video_source(MediaResource, Stream),
	 sub_atom(Stream, Before, _, _, domain),
	 sub_atom(Stream, End, _, _, rawvideo),
	 Length is End-Before,
	 sub_atom(Stream, Before, Length, _,  DomainPath),
	 atomic_list_concat([Server, DomainPath, 'shots/1/'], URL),
	 !.
mediaresource_image_source(_MediaResource, '').



time_property(Type, R, Time) :-
	(   Type = start
	->  rdf(R, nsa:temporalStart, TimeLit)
	;   Type = end
	->  rdf(R, nsa:temporalEnd, TimeLit)
	),
	literal_text(TimeLit, TimeAtom),
	(   atom_number(TimeAtom, TimeNumber)
	->  Time is TimeNumber
	;   Time = TimeAtom
	).

