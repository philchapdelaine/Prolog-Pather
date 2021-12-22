![Our KB for our POC](https://github.students.cs.ubc.ca/mpennefa/PrologProject/edit/master/Pictures/KB.PNG?raw=true)
![Full Trace for Nest input](https://github.students.cs.ubc.ca/mpennefa/PrologProject/edit/master/Pictures/Nest.PNG?raw=true)
![Full Trace for IKB input](https://github.students.cs.ubc.ca/mpennefa/PrologProject/edit/master/Pictures/IKB.PNG?raw=true)
![Full Trace for Bookstore input](https://github.students.cs.ubc.ca/mpennefa/PrologProject/edit/master/Pictures/Bookstore.PNG?raw=true)

OUR PROJECT WAS WORKING BUT THE VERSION CHANGE CAUSED ERRORS:
Before the SWIPL rollback, calling "start." and then selecting a destination "Nest.","IKB.", or "Bookstore." would return the destination with the shortest time taken on the first call, regardless of the order they were defined. However, after the rollback, it now sometimes selects the first one defined, regardless of time taken.


# PrologPather 

PrologPather is a pathfinding application designed to help the user find the quickest path from point A to point B (As long as points A and B are on UBC campus), implemented entirely in Prolog. It works by comparing time taken values of individual sections of routes, and also features options such as including a scenic route, or stopping for a cafe/restaurant.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is: 
+ Michael Pennefather (student # 1): optional awesome nickname 1
+ Philippe Chapdelaine (student # 2): optional awesome nickname 2
+ Yashmeet Malhotra (student # 3): optional awesome nickname 

We call ourselves: Optional Team Title

## Product Pitch


Post lockdown, many students are returning to UBC campus. For both new students and returning students, navigating around UBC campus efficiently can be a real challenge, with many unnecessarily long trips to get to classes. PrologPather is a pathfinding application designed to help the user find the quickest path from point A to point B (As long as points A and B are on UBC campus), implemented entirely in Prolog. We aim to use Prolog’s strength in logical programming to recommend the shortest (in terms of time) possible path through UBC campus, based on a knowledge base filled with UBC’s many pedestrian paths and their respective times. We would use the CLPFD/Constraint Logical Programming package to compare integer time values of different routes and select the shortest ones.

Our program would assist students to get to classes on time, as well as helping new visitors to the campus find their way easily. It would also be able to take into account needs or wants specified by the user, such as avoiding traffic or having a stop (gas station, restaurant, scenic views etc.) along the way. 

Prolog allows us to establish a detailed knowledge base by defining connections beforehand, making for easy comparisons to find the best path. Since there might be many paths to the same destination, it makes it much easier to have the results be based on logical criteria either built into the program or established by the user (shortest path, more scenery, public transport only, etc). 


## Minimal Viable Project

Our minimum viable project would be to create an app that would:
- Given a starting location A and a destination location B, be able to return the shortest (in terms of time) fully connected path between A and B, and complete directions.
- Contains an extensive local database of all major locations on UBC campus and their appropriate directions and estimated travel times.
- Allows users to print different locations from said database by filtering by a category: E.g. “show Libraries” returns IKB, Koerner, Woodward, etc...

This functionality would allow our application to solve the problem outlined above, by giving users an easy way to efficiently navigate through UBC.
Our MVP will take advantage of Prolog’s strength in logical programming, to quickly and efficiently compare times and ensure all paths are properly connected, as we will be able to define all connected paths in our knowledge base. We can also define time taken for each stretch of path, and easily add together those paths and compare final routes to find the fastest directions using CLPDG.

## Proof of Concept
Our proof of concept focuses on the UI of the project, and allows users to [input their destination](https://github.students.cs.ubc.ca/mpennefa/PrologProject/blob/0726742e380b8819c339241e63e824650fa9b205/prologpather.pl#L7)and have [the directions as well as the time taken be returned to them](https://github.students.cs.ubc.ca/mpennefa/PrologProject/blob/0726742e380b8819c339241e63e824650fa9b205/prologpather.pl#L17). Having a clean and functional input interface is essential to the project, as our mapping service is not very useful if users can’t choose where they want to go. 

Additionally, the proof of concept contains a basic form of path length comparison, using the CLP package to pick the shorter of two paths that lead to the same location, highlighting how we would implement it on a large scale for the final project.

Completing the command line UI allows us to focus development on the pathing system, which would be the only requirement left for the MVP, after I/O and the knowledge base formatting have already been completed.



### How to test and run the code: Prolog

To run code: Simply CD into the directory the file is in, open it with "swipl prologpather.pl", and enter "start." to begin. The program will prompt you to enter the case-sensitive name of one of the listed destinations, and upon doing so you will receive the directions to get to the destination, assuming you are starting from the university boulevard bus loop.

We have set up a simple test file for you to extend using [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing. Included among your tests should be some that demonstrate the core functionality of your code. Please remove the example tests before you submit or you will lose marks. (We will be running `make prolog-eval` from the project root.)

In the `prolog` directory, you can run `make test` to run the unit tests. You can also load the test file into the swipl repl with `make test-repl` and in that repl you can run `run_tests.` to run those tests.
Alternatively, you can load the test file into the swipl repl with `swipl test.pl` and in that repl you can run `run_tests.` to run our tests.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!


