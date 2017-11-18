:- module(links_app, []).

:- use_module(http_service, [get_html/2]).
:- use_module(recept_service, [parse_links/2,
                               base_url/1,
                               search_url/1]).

list_links :-
  base_url(Base),
  search_url(Rel),
  full_url(Base, Rel, Url),

  load_links_url(Url, Links),
  writeq(Links).


load_links_url(Url, Recipe) :-
  %load_html('test.html', Contents, []),
  get_html(Url, Contents),
  !,
  parse_links(Contents, Recipe).



full_url(BaseUrl, Relative, Url) :-
  string_concat(BaseUrl, Relative, Url).
