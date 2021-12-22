:- use_module(library(clpfd)).
:- discontiguous destination/2.
:- discontiguous food/1.
:- discontiguous study_space/1.
:- discontiguous sports/1.
:- discontiguous food/1.
:- discontiguous library/1.
:- discontiguous gym/1.
:- discontiguous housing/1.
:- discontiguous study_space/1.
:- discontiguous lecture_hall/1.
:- discontiguous scenic/1.
:- discontiguous parking/1.
:- discontiguous museum/1.
:- discontiguous medical/1.
:- discontiguous edge/3.
:- discontiguous intersection/2.


%Call "start." to begin the pathing process.
start :-
    writeln("Welcome to PrologPather! Please input a starting location, destination category, or input `options` to see destination categories."),
    read_line_to_string(current_input, Input),
    goto(Input).


%Get the description from the ID of a "node" in our map graph
pull_description(Id,Description) :- destination(Id,Description).
pull_description(Id,Description) :- intersection(Id,Description).


%   print_directions/1: Prints a set of directions from a list of node IDs.
%Last 2 elements case: Concatenate strings together to output "Finally, from <2nd last element>, head to <Last element>"
print_directions([SecondLast,Last]) :-
    pull_description(SecondLast,SecondDesc),
    pull_description(Last,LastDesc),
    string_concat("Finally, from ",SecondDesc,String1),
    string_concat(String1,", head to ",String2),
    string_concat(String2, LastDesc, OutString),
    writeln(OutString).

%Recursive case: Concatenate strings together to output "Head towards <current element>" then recurse on rest of list.
print_directions([First|Rest]) :-
	pull_description(First,Desc),
    string_concat("Head towards ",Desc,OutString), writeln(OutString),
    print_directions(Rest).





%   goto/1: Given a user's string input, either:
%       - print the directions and time, list out destination categories
%       - print out all destination categories
%       - print out all destinations belonging to a category
%       - otherwise ask for a valid input

%if Input matches a destination's ID, ask for a second destination to path to.
goto(Input) :- destination(ID,Input),
    writeln("Now please input a destination to path to, `options` to view destinations again, or `back` to re-select your starting point."),
    read_line_to_string(current_input, Input2),
    % If the user dosen't enter a proper second destination, the whole process restarts from goto(), so as to avoid locking users into one destination.
    goto_dest(Input,Input2).
%if Input matches a category or "options", print the appropriate category then ask for input again.
goto(Category) :-
    print_category(Category),
    writeln("Please input a valid destination, or input `options` to see destination categories."),
    read_line_to_string(current_input, Input),
    goto(Input).
%Fail case - No matching destination or category found
goto(_) :- writeln("Please input a valid destination, or input `options` to see destination categories."),read_line_to_string(current_input, Input2), goto(Input2).



%   goto_dest/2: Helper function that takes an input, Start, from goto/1 and returns directions to a new input, End.

%if End matches a destination's ID, ask for find the shortest path and print out directions and time taken.
goto_dest(Start,End) :-
    destination(ID1,Start),
    destination(ID2,End),
    shortest_path(ID1,ID2,P,T),
    %Concatenate strings together: "To get to <Destination>, "
    string_concat("To get to ",End,IntroStr),
    writeln(IntroStr),
    %Call helper to print directions
    print_directions(P),
    %Concatenate strings: "Total time taken: <Time> minutes."
    number_string(T,Tstring),
    string_concat(Tstring," minutes.", MinsTaken),
    string_concat("Total time taken: ",MinsTaken,OutroStr),
    writeln(OutroStr).

%If input matches a category or options, print the appropriate category then ask for input again.
goto_dest(Start,Category) :-
    print_category(Category),
    read_line_to_string(current_input, Input),
    goto_dest(Start,Input).

%If input is "back", return to goto() to allow user to re-input starting point.
goto_dest(_,"back") :-
    goto("invalid").

%Fail case - No matching destination or other input found
goto_dest(Start,_) :-
    writeln("Please input a valid destination, `options` to see destination categories, or `back` to re-select your starting location."),
    read_line_to_string(current_input, Input2),
    goto_dest(Start,Input2).





%Helper that prints either prints all categories or all destinations in a category.

%Print destination categories
print_category("options") :-
    writeln("Input a category to see all destinations in that category."),
    findall(X, category(_,X), L),
    print_list(L).

%Print destinations in associated category.
print_category("All") :-
    findall(X, destination(_,X), L),
    print_list(L).

print_category("Food") :-
    findall(String, (food(X),destination(X,String)), L),
    print_list(L).

print_category("Study Spaces") :-
    findall(String, (study_space(X),destination(X,String)), L),
    print_list(L).

print_category("Sports/Gyms") :-
    findall(String, (sports(X),destination(X,String)), L),
    print_list(L).

print_category("Libraries") :-
    findall(String, (library(X),destination(X,String)), L),
    print_list(L).

print_category("Housing") :-
    findall(String, (housing(X),destination(X,String)), L),
    print_list(L).

print_category("Lecture Halls") :-
    findall(String, (lecture_hall(X),destination(X,String)), L),
    print_list(L).

print_category("Scenic") :-
    findall(String, (scenic(X),destination(X,String)), L),
    print_list(L).

print_category("Parking") :-
    findall(String, (parking(X),destination(X,String)), L),
    print_list(L).

print_category("Museums") :-
    findall(String, (museum(X),destination(X,String)), L),
    print_list(L).

print_category("Medical") :-
    findall(String, (medical(X),destination(X,String)), L),
    print_list(L).




%focus on the "making a graph" and "finding the shortest path"
%"We're representing advanced graph data structures and building algorithms to path through them in the most efficient way"
%


%recurisvely print a list of strings
print_list([FirstDest|RestDest]) :- writeln(FirstDest),print_list(RestDest).
print_list([]) :- writeln("").



%   path_to/4:
%       Point A:start of Path
%       Point B:end of Path
%       Path: List of nodes, in order, to get from Point A to Point B
%       Time: Total time taken to get from A to B.

%-- adapted from J.R Fisher's's path from:
%-- https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15.html

path_to(PointA,PointB,Path,Time) :-
    %helper function that does all the work
    travel(PointA,PointB,[PointA],RPath,Time),
    %Just reverse the path that travel/5 returns - this is done because of how our cons operator works
    reverse(RPath,Path).


%Base case: A and B are adjacent
travel(A,B,P,[B|P],Time) :-
    connected(A,B,Time).
%Recursive case: A and B are not adjacent
travel(A,B,Visited,Path,Time) :-
    connected(A,C,T1),
    C \== B,
    \+member(C,Visited),
    %A is connected to some node C that isn't B, and hasn't been visited
    travel(C,B,[C|Visited],Path, NewTime),
    Time is T1 + NewTime.
    %Add C to the visited nodes, increase the time, and recurse



shortest_path(A,B,Path,Time) :-
    %Return the path from A to B who's time is =< all other path's times
    path_to(A,B,Path,Time),
    \+ (path_to(A,B,_,Time2), (Time > Time2)).



%edges on the graph are non-directional
connected(X,Y,T) :- edge(X,Y,T) ; edge(Y,X,T).







%---------------------Knowledge base:---------------------

%Node categories
category(destination,"All").
category(food, "Food").
category(study_space, "Study Spaces").
category(sports, "Sports/Gyms").
category(library, "Libraries").
category(housing, "Housing").
category(lecture_hall, "Lecture Halls").
category(scenic, "Scenic").
category(parking, "Parking").
category(museum, "Museums").
category(medical, "Medical").

%---------------------Nodes---------------------
%Destinations + the categories they belong to
destination(browns,"Browns Socialhouse").
    food(browns).
destination(downlow,"Downlow Chicken").
    food(downlow).
destination(shoppers_univBlvd,"Shoppers UnivBlvd").
    food(shoppers_univBlvd).
    medical(shoppers_univBlvd).
destination(kinton,"Kinton Ramen").
    food(kinton).
destination(nest,"The Nest").
    study_space(nest).
    food(nest).
destination(swng,"Swing Space").
    lecture_hall(swng).
destination(ikb,"IKB").
    study_space(ikb).
    food(ikb).
    library(ikb).
destination(life,"Life Building").
    study_space(life).
    food(life).
    sports(life).

%Intersections
intersection(univBlvd_eMall,"University Blvd and East Mall").
intersection(univBlvd_mainMall,"University Blvd and Main Mall").
intersection(univBlvd_wMall,"Univeristy Blvd and West Mall").
intersection(univBlvd_wesbMall,"Univeristy Blvd and Wesbrook Mall").
intersection(eMall_agriRd,"East Mall and Agricultural Road").
intersection(eMall_memoRd,"East Mall and Memorial Road").



%---------------------Edges---------------------

%University Blvd bus loop
edge(nest,univBlvd_eMall,1).
edge(nest,life,0.5).
edge(univBlvd_eMall,browns,1).
edge(univBlvd_eMall,downlow,1).
edge(univBlvd_eMall,kinton,1).
edge(univBlvd_eMall,shoppers_univBlvd,2).
edge(univBlvd_wesbMall,browns,1).
edge(univBlvd_wesbMall,downlow,1).
edge(univBlvd_wesbMall,kinton,1).
edge(univBlvd_wesbMall,shoppers_univBlvd,2).

%Univeristy Boulevard cont
edge(univBlvd_mainMall,univBlvd_eMall,3).
edge(univBlvd_mainMall,univBlvd_wMall,3).
edge(univBlvd_wMall,swng,3).

%East mall
edge(univBlvd_eMall,eMall_agriRd,2).
edge(eMall_agriRd,ikb,1).
edge(eMall_agriRd,eMall_memoRd,2).
edge(eMall_agriRd,life,2).
edge(eMall_memoRd,ikb,2).






destination(kenWoodsField,"Ken Woods Field").
    sports(kenWoodsField).
destination(ubcBaseballField,"UBC Baseball Field").
    sports(ubcBaseballField).
destination(thunderbirdStadium,"Thunderbird Stadium").
    sports(thunderbirdStadium).
destination(rashpalDhillonTFOval,"Rashpal Dhillon Oval").
    sports(rashpalDhillonTFOval).
destination(nationalSoccerDevCentre,"Soccer Development Centre").
    sports(nationalSoccerDevCentre).
destination(warrenField,"Warren Field").
    sports(warrenField).
destination(wrightField,"Wright Field").
    sports(wrightField).
destination(wolfsonEastField,"Wolfson East Field").
    sports(wolfsonEastField).
destination(frankBuckField,"Frank Buck Field").
    sports(frankBuckField).
destination(dMThunderbirdSportsCentre,"Doug Mitchell Thunderbird Centre").
    sports(dMThunderbirdSportsCentre).
destination(ubcTennisCentre,"UBC Tennis Centre").
    sports(ubcTennisCentre).
destination(ubcMiniField,"UBC Mini Field").
    sports(ubcMiniField).

destination(totemParkResidence, "Totem Park Residence").
    housing(totemParkResidence).
    study_space(totemParkResidence).
destination(promontory, "Promontory").
    housing(promontory).

destination(beanAroundTheWorld, "Bean Around The World").
    food(beanAroundTheWorld).

destination(thunderbirdParking,"Thunderbird Parking").
    parking(thunderbirdParking).
destination(hawthornParking,"Hawthorn Parking").
    parking(hawthornParking).

destination(sJohnHospice,"St. John Hospice").
    medical(sJohnHospice).

destination(gardenPavilion, "Garden Pavilion").
    scenic(gardenPavilion).
destination(carolinianForestGarden, "Carolinian Forest Garden").
    scenic(carolinianForestGarden).

intersection(sWMa_w16Ave, "South West Marine Dr and West 16th Ave").
intersection(sWMa_stadRd,"South West Marine Dr and Stadium Rd").
intersection(wMall_stadRd,"West Mall and Stadium Rd").
intersection(eMall_stadRd,"East Mall and Stadium Rd").
intersection(eMall_w16Ave,"East Mall and West 16th Ave").
intersection(wesbMall_w16Ave,"Wesbrook Mall and West 16th Ave").
intersection(wMall_thunBlvd, "West Mall and Thunderbird Blvd").
intersection(larkDr_thunBlvd, "Larkin Dr and Thunderbird Blvd").
intersection(eMall_logaLn,"East Mall and Logan Ln").
intersection(eMall_eaglDr,"East Mall and Eagles Dr").
intersection(eMall_thunBlvd, "East Mall and Thunderbird Blvd").
intersection(eaglDr_thunBlvd, "Eagles Dr and Thunderbird Blvd").
intersection(wMall_larkDr, "West Mall and Larkin Dr").
intersection(wesbMall_thunBlvd, "Wesbrook Mall and Thunderbird Blvd").

%Intersection connection edges
edge(sWMa_w16Ave, eMall_w16Ave, 6).
edge(eMall_w16Ave, eMall_stadRd, 9).
edge(eMall_stadRd, eMall_eaglDr, 5).
edge(eMall_eaglDr, eMall_thunBlvd, 4).
edge(eaglDr_thunBlvd, eMall_eaglDr, 4).
edge(eaglDr_thunBlvd, larkDr_thunBlvd, 3).
edge(larkDr_thunBlvd, wMall_thunBlvd, 2).
edge(wMall_thunBlvd, wMall_larkDr, 2).
edge(wMall_larkDr, wMall_stadRd, 2).
edge(wMall_stadRd, sWMa_stadRd, 2).
edge(wMall_stadRd, eMall_stadRd, 5).
edge(sWMa_stadRd, sWMa_w16Ave, 3).
edge(eMall_logaLn, eMall_stadRd, 2).
edge(eMall_logaLn, eMall_eaglDr, 2).
edge(eMall_logaLn, eMall_thunBlvd, 1).
edge(wesbMall_w16Ave, eMall_w16Ave, 6).
edge(wesbMall_thunBlvd, wesbMall_w16Ave, 9).

% Sports complex edges
edge(kenWoodsField, eMall_eaglDr, 1).
edge(ubcBaseballField, eMall_w16Ave, 2).
edge(thunderbirdStadium, wMall_stadRd, 2).
edge(rashpalDhillonTFOval, wesbMall_w16Ave, 2).
edge(nationalSoccerDevCentre, wesbMall_w16Ave, 3).
edge(warrenField, eMall_stadRd, 0.5).
edge(wrightField, eMall_stadRd, 1).
edge(wolfsonEastField, eMall_logaLn, 0.5).
edge(frankBuckField, eMall_logaLn, 1).
edge(dMThunderbirdSportsCentre, wesbMall_thunBlvd, 2).
edge(ubcTennisCentre, eMall_logaLn, 3).
edge(ubcMiniField, eMall_stadRd, 1).

edge(thunderbirdParking, wMall_stadRd, 2).
edge(hawthornParking, eMall_eaglDr, 2).

edge(gardenPavilion, sWMa_w16Ave, 2).
edge(carolinianForestGarden, sWMa_stadRd, 2).

edge(sJohnHospice, wMall_stadRd, 0.5).

edge(beanAroundTheWorld, larkDr_thunBlvd, 1).

edge(totemParkResidence, wMall_thunBlvd, 4).
edge(promontory, wMall_stadRd, 2).


% Other third
destination(corpusChristiCollege, "Corpus Christi College").
destination(ionaBuilding, "Iona Building").
destination(careyCollege, "Carey College").
destination(walterGage, "Walter Gage Residence").
destination(timHortons, "Tim Hortons").
destination(lawBuilding, "Peter Allard Law Building").
destination(lifeBuilding, "UBC Life Building").
destination(src, "Student Recreation Centre").

intersection(eMall_chanBlvd, "East Mall and Chancellor Blvd").
intersection(eMall_waltRd, "East Mall and Walter Gage Road").
intersection(wesbMall_chanBlvd, "Wesbrook Mall and Chancellor Blvd").
intersection(wesbMall_ionaDr, "Wesbrook Mall and Iona Dr").
intersection(wesbMall_waltRd, "Wesbrook Mall and Walter Gage Road").
intersection(wesbMall_studBlvd, "Wesbrook Mall and Student Union Blvd").
intersection(wesbMall_ubcBusLoop, "Wesbrook Mall and UBC Bus Loop").
intersection(stAndrewsWalk_ionaDr, "St Andrews Walk and Iona Drive").
intersection(stAndrewsWalk_waltRd, "St Andrews Walk and Walter Gage Rd").

edge(wesbMall_chanBlvd, wesbMall_ionaDr, 1.5).
edge(wesbMall_ionaDr, wesbMall_waltRd, 2).
edge(wesbMall_waltRd, wesbMall_studBlvd, 3).
edge(wesbMall_studBlvd, wesbMall_ubcBusLoop, 2).
edge(wesbMall_ubcBusLoop, univBlvd_wesbMall, 2).
edge(wesbMall_chanBlvd, eMall_chanBlvd, 7).
edge(eMall_chanBlvd, eMall_waltRd, 3).
edge(eMall_waltRd, stAndrewsWalk_waltRd, 1.5).
edge(stAndrewsWalk_waltRd, wesbMall_waltRd, 2.5).
edge(stAndrewsWalk_waltRd, stAndrewsWalk_ionaDr, 1).
edge(stAndrewsWalk_ionaDr, wesbMall_ionaDr, 3).

edge(corpusChristiCollege, wesbMall_chanBlvd, 0.5).
edge(ionaBuilding, stAndrewsWalk_ionaDr, 1).
edge(careyCollege, wesbMall_ionaDr, 1).
edge(walterGage, wesbMall_studBlvd, 2).
edge(walterGage, stAndrewsWalk_waltRd, 1).
edge(timHortons, wesbMall_studBlvd, 3).
edge(lawBuilding, eMall_chanBlvd, 1.5).
edge(lifeBuilding, wesbMall_studBlvd, 3).
edge(lifeBuilding, wesbMall_ubcBusLoop, 3).
edge(src, wesbMall_studBlvd, 3).

%connect to Michael
edge(univBlvd_eMall, eMall_waltRd, 7).
edge(eMall_waltRd, eMall_agriRd, 3).

%connect to Yash
edge(wesbMall_thunBlvd, eMall_thunBlvd, 5).
edge(eMall_thunBlvd, healMall_agroRd, 2).
edge(wesbMall_thunBlvd, agroRd_wesbMall, 1.5).
edge(eMall_waltRd, mainMall_cresRd, 3).
edge(eMall_waltRd, mainMall_memoRd, 3).
edge(eMall_waltRd, eMall_agriRd, 3).

edge(univBlvd_eMall, applLn_eMall, 4).

% Yash's
lecture_hall(cemeBldng).
study_space(cemeBldng).
edge(cemeBldng, applLn_eMall, 1).
destination(cemeBldng, "CEME").

scenic(bookStore).
edge(bookStore, univBlvd_eMall, 0.1).
destination(bookStore, "UBC Bookstore").

edge(univBlvd_mainMall,univBlvd_eMall,3).
edge(univBlvd_eMall,eMall_agriRd,2).
edge(eMall_agriRd,eMall_memoRd,2).
edge(eMall_agriRd, agriRd_mainMall,2).

intersection(storRd_mainMall,"Stores Road and Main Mall").
intersection(storRd_wMall, "Stores Road and West Mall").
edge(storRd_mainMall, storRd_wMall, 4).
edge(storRd_mainMall, applLn_eMall, 4).

intersection(agroRd_wesbMall, "Agronomy Road and Westbrook Mall").
intersection(healMall_agroRd, "Health Sciences Mall and Agronomy Road").
intersection(wesbMall_thunBlvd, "Westbrook Mall and Thunderbird Boulevard").
intersection(applLn_eMall, "Applied Sciences Lane and East Mall").
edge(agroRd_wesbMall, healMall_agroRd, 2).
edge(healMall_agroRd, wesbMall_thunBlvd, 6).
edge(healMall_agroRd, applLn_eMall, 5).

intersection(wMall_agroRd, "West Mall and Agronomy Road").
intersection(univBlvd_eMall,"University Blvd and East Mall").
intersection(univBlvd_mainMall,"University Blvd and Main Mall").
intersection(univBlvd_wMall,"Univeristy Blvd and West Mall").
intersection(univBlvd_wesbMall,"Univeristy Blvd and Wesbrook Mall").
intersection(eMall_agriRd,"East Mall and Agricultural Road").
intersection(eMall_memoRd,"East Mall and Memorial Road").
