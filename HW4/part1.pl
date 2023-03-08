 
:- dynamic(student/3).
:- dynamic(class/3).
:- dynamic(course/4).
:- dynamic(instructor/3).
:- style_check(-singleton).
student(s1,[course1],no).
student(s3,[course3],handicapped).
instructor(i1,[course1,course5],[sth]).
instructor(i2,[course2,course6],[sth,projector]).
course(c1,13,i1,[handicapped]).
course(c2,25,i2,[projector,sth]).
course(c3,50,i2,[projector,sth]).
class(class1,15,[sth,handicapped,no]).
class(class2,10,[projector,no]).
occupied(c1,class1,8).
occupied(c1,class1,9).
occupied(c3,class1,9).
enrollStudent(ID,Courses,Needs):-    \+student(ID,_,_),    assertz(student(ID,Courses,Needs)).
appendClass(ID,Capacity,Equipments):-    \+class(ID,_,_),    assertz(class(ID,Capacity,Equipments)).
appendCourse(ID,Capacity,InstructorID,Needs):-    \+course(ID,_,_,_),    assertz(course(ID,Capacity,InstructorID,Needs)).
courseOfStudent(Course,Class):-    course(Course,C1,_,Needs),    class(Class,C2,Equipments),    C1 =< C2,     subset(Needs,Equipments).
classOfStudent(Student,Class):-        student(Student,_,Needs),    class(Class,_,Equipments),    in(Needs,Equipments). 
conflictsForCourses(Course1,Course2):-    occupied(Course1,C1,T1),    occupied(Course2,C2,T2),    not(Course1=Course2),    C1=C2,(T1=T2).  
in(E, [E|Rest]).
in(E, [I|Rest]):-	in(E, Rest).
subset([], B).
subset([E|Rest], B):-	element(E, B),	subset(Rest, B). 
	
element(E, S):-in(E, S).
