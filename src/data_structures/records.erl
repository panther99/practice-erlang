-module(records).
-include("records.hrl").
-author("Nikola Stojaković").

-export([
    basic_student/0,
    admin_panel/1,
    adult_section/1,
    teach/1,
    included/0
]).

-record(student, {name,
                  grade,
                  class,
                  age=18,
                  subjects=[]}).
-record(user, { id, name, group, age }).

basic_student() ->
    #student{name="Nikola",
             grade=4,
             class=1,
             age=21}.

teach(Student) ->
    Subjects = Student#student.subjects,
    NewStudent = Student#student{subjects = [maths, physics | Subjects]},
    {taught, NewStudent}.

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed.";

admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed.".

adult_section(U = #user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.

included() -> #included{first_field="yay"}.
