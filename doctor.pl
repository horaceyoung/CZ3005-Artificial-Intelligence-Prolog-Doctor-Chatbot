/*Mark the following knowledge base as dynamic to modify them in the program*/
:- dynamic(askedsymp/1).
:- dynamic(has/1).
:- dynamic(hasnot/1).
:- dynamic(patientmood/1).
:- dynamic(patientpain/1).

/*The begin predicate serves as an initialization. It will remove the past diagnosis history and print out a welcome message, then continue to the procedure askMood to record the patient/'s mood*/
begin():-
	retractall(has(_)),
	assert(has([])),
	retractall(hasnot(_)),
	assert(hasnot([])),
	retractall(askedsymp(_)),
	assert(askedsymp([])),
	format('Doctor: ~n Welcome to the sympathetic doctor, we will begin the diagnose process now. ~nWe will ask you a few questions about your cnditions and symptoms. ~nIf you feel like you have provided enough information, enter \'d\' to diagnose. ~nOtherwise, enter \'q\' to abort the session ~n'),
	moodlist(L),
	askMood(L).

/*This procedure gets the current patient mood and patient pain, use a weighted sum of mood and pain and pass the result to chooseTone() to intellectually choose a appropriate tone for the doctor.
Here I have set the default formula to D*0.2+S*0.8 as the seriousness indicator, but the fractions are free to change if necessary. Higher the value, more concerned the doctor would be.
*/	
sympatheticAsk():-
	format('Doctor: '),
	patientmood(M),
	mood(D, M),
	patientpain(P),
	pain(S, P),
	chooseTone(D*0.2+S*0.8).

/*Based on the argument D, which is the seriousness indicator, one of the Gestures will be called.*/
chooseTone(D):-
	(D=<1 -> normalGesture();
	 D=<3 -> politeGesture();
	 D=<5 -> calmingGesture()
	),
	format(': ').

/*Gestures of a kind is randomly chosen*/
normalGesture():- norgesture(L), random_member(X, L), format('* ~w *~n', [X]).
politeGesture():- calgesture(L), random_member(X, L), format('* ~w *~n', [X]).
calmingGesture():- polgesture(L), random_member(X, L), format('* ~w *~n', [X]).

/*Doctor speaks with a tone. If mood list has run out, terminate the progam. Else, according to the answer of the patient, the program either aborts, enters diagnose mood if answer is y,
recursively ask the next mood if answer is no, or begin diagnosis.*/
askMood(L):-
	sympatheticAsk(),
	list_empty(L, V),
	(V == true -> format('Unfortuanetely we could not diagnose you if you do not provide credible answers to our questions. Try again later. ~n');
	 L = [H|T]
	),
	format('May I ask do you have a ~w mood today? y/n/q/d: ~n', [H]),
	read(AnswerMood),
	(AnswerMood == q -> abort;
	 AnswerMood == y -> diagnoseMood(H);
	 AnswerMood == n -> askMood(T);
	 AnswerMood == d -> diagnosebegin()).

/*This clears the default patientmood and records the user input.*/
diagnoseMood(M):- retractall(patientmood(_)), assert(patientmood(M)), painlist(L), askPain(L).

/*Similar to askMood*/
askPain(L):-
	sympatheticAsk(),
	list_empty(L, V),
	(V == true -> format('Sorry we could not diagnose you if you do not provide credible answer to our questions. Try again later. ~n');
	 L = [H|T]
	 ),
	format('May I ask do you have ~w today? y/n/q/d: ~n', [H]),
	read(AnswerPain),
	(AnswerPain == q -> abort;
	 AnswerPain == y -> diagnosePain(H);
	 AnswerPain == n -> askPain(T);
	 AnswerPain == d -> diagnosebegin()).

/*Similar to diagnoseMood, this clears the default patient pain which is no pain and records the user input. Additionally this enters next phase askSymptom.*/
diagnosePain(P):- retractall(patientpain(_)), assert(patientpain(P)), askSymptom('cough').

/*Similar to askMood. If answer is y, enters affirmSymptom, and if answer if n, enters negateSymptom*/
askSymptom(X):-
	sympatheticAsk(),
	format("Do you ~w ? y/n/q/d: ~n", [X]),
	read(AnswerSymptom),
	(AnswerSymptom == q ->abort();
	 AnswerSymptom == y -> affirmSymptom(X);
	 AnswerSymptom == n -> negateSymptom(X);
	 AnswerSymptom == d -> diagnosebegin()
	 ).	 

/*Entering this procedure means the patient has confirmed a symptom. has() will be updated and askedsymp(), which contains all asked symptoms will be updated as well.
Based on the symptom, a related symptom will be asked. If all symptoms of a given disease has been confirmed by the patient, end asking and enter diagnose procedure.*/
affirmSymptom(X):-
	has(H), append(H, [X], HX), assert(has(HX)), retract(has(H)),
	askedsymp(L), append(L, [X], LX), assert(askedsymp(LX)), retract(askedsymp(L)),
	findall(Y,(related(X, Y), askedsymp(N), \+member(Y, N)), R),
	(R == [] -> diagnosebegin();
	 random_member(Q, R)),
	askSymptom(Q).

/*Similar to affirmSymptom, but all effects are negative. hasnot() will be updated and askedsymp() will be updated as well. A random unasked symptom from the symptom poll will be asked.
If no more symptoms are unasked, enter the diagnose procedure.*/
negateSymptom(X):-
	hasnot(N), append(N, [X], NX), assert(hasnot(NX)), retract(hasnot(N)),
	askedsymp(L), append(L, [X], LX), assert(askedsymp(LX)), retract(askedsymp(L)),
	cold(C), diabetes(D), hiv(H), lungcancer(K), rabies(B),
	append(C, D, CD), append(CD, H, CDH), append(CDH, K, CDHK), append(CDHK, B, CDHKB),
	askedsymp(S),
	findall(Y,(member(Y,CDHKB), \+member(Y, S)), R),
	(R == [] -> diagnosebegin();
	random_member(Q, R)),
	askSymptom(Q).

/*Begin diagnose. Print a message.*/
diagnosebegin():-
	sympatheticAsk(),
	format('Thank you! We have completed the diagnosis process.~nFollowing is our analysis: ~n'),
	diagnose().

/*Diagnose if either or all of the five diseases are diagnosed.*/
diagnose():-
	diagnosecold();
	diagnosediabetes();
	diagnosehiv();
	diagnoselungcancer();
	diagnoserabies();
	format('We could not draw any conclusion based on the information you provided. Please try asgain later.~n'),
	diagnoseEnd().
	

/*The following five clauses will compare the symptoms in has() and the symptoms a disease possess, and if more than 4 symptoms are matched, a diagnosis message will be printed.*/
diagnosecold():-
	has(H),cold(C), intersection(H, C, R), length(R, L),
	(L >= 4 -> format('you might have cold.~n')),
	diagnoseEnd().
	
diagnosediabetes():-
	has(H),diabetes(C), intersection(H, C, R), length(R, L),
	(L >= 4 -> format('you might have diabetes.~n')),
	diagnoseEnd().

diagnosehiv():-
	has(H),hiv(C), intersection(H, C, R), length(R, L),
	(L >= 4 -> format('you might have HIV.~n')),
	diagnoseEnd().

diagnoselungcancer():-
	has(H),lungcancer(C), intersection(H, C, R), length(R, L),
	(L >= 4 -> format('you might have lung cancer.~n')),
	diagnoseEnd().

diagnoserabies():-
	has(H),rabies(C), intersection(H, C, R), length(R, L),
	(L >= 4 -> format('you might have rabies.~n')),
	diagnoseEnd().

/*End of program*/
diagnoseEnd():-
	format("Thank you for using our service! You can consult another disease now. ~n"),
	abort().

/*Y is related to X if they are both symptoms under one disease*/
related(X, Y):-	
	cold(L), member(X, L), member(Y, L);
	diabetes(L), member(X, L), member(Y, L);
	hiv(L), member(X, L), member(Y, L);
	lungcancer(L), member(X, L), member(Y, L);
	rabies(L), member(X, L), member(Y, L).

/*Randomly choose a symptom from the poll of symptoms*/
randomSymptom(X):-
	cold(C), diabetes(D), hiv(H), lungcancer(L), rabies(R),
	append(C, D, CD), append(CD, H, CDH), append(CDH, L, CDHL), append(CDHL, R, CDHLR),
	random_member(X, CDHLR).

/*A procedure to check if the list is empty*/
list_empty([], true).
list_empty([_|_], false).


/*Below are the default data of mood, pain, and disease symptoms.*/
moodlist([calm, tense, melancholy, gloomy, fearful]).
painlist(['no pain', 'mild pain', 'moderate pain', 'severe pain', 'intolerable pain']).

norgesture(['broad_smile', 'joke', 'beaming_voice']).
polgesture(['looks concerned','mellow voice','light touch','faint_smile']).
calgesture(['greet', 'look_composed', 'look attentive']).

cold(['sneeze', 'cough', 'have congestion', 'have mild headache', 'have stuffy nose']).
diabetes(['feel very thirsty', 'have extreme fatigue', 'have weight loss', 'have blurry vision', 'urinate often']).
hiv(['have muscle aches', 'have sore throat', 'have fatigue', 'have swollen lymph nodes', 'have mouth ulcers']).
lungcancer(['cough blood', 'have chest pain', 'have hoarseness', 'lose appetite', 'have shortness of breath']).
rabies(['feel easily irritated', 'have hallucinations', 'experience excessive movements', 'have extreme sensitivity to bright lights', 'have convulsions']).

/*Rate the seriousness of the mood and pain, with 1 being the least serious and 5 being the most serious*/
mood(5, melancholy).
mood(4, fearful).
mood(3, gloomy).
mood(2, tense).
mood(1, calm).
pain(5, 'intolerable pain').
pain(4, 'severe pain').
pain(3, 'mild pain').
pain(2, 'moderate pain').
pain(1, 'no pain').

/*Initialization*/
askedsymp([]).
has([]).
hasnot([]).

/*Default patient mood and patient pain*/
patientmood(calm).
patientpain('no pain').