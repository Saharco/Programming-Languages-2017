/**
	Sahar Cohen 206824088 saharco@campus.technion.ac.il
	Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il
*/

/***
@descr Family rules.
@author Tomer
@date Long time ago
*/

/**
@form male(Name).
@constraints
  @unrestricted Name.
@descr Person with Name is a male.
*/
male(terah).
male(haran).
male(lot).
male(abraham).
male(ishmael).
male(isaac).
male(bethuel).
male(laban).
male(jacob).
male(iscah).
male(esau).
male(reuben).
male(simeon).
male(levi).
male(judah).
male(dan).
male(naphtali).
male(gad).
male(asher).
male(issachar).
male(zebulun).
male(joseph).
male(benjamin).
male(tomer).
male(nahor).


/**
@form female(Name).
@constraints
  @unrestricted Name.
@descr Person with Name is a female.
*/
female(sarah).
female(hagar).
female(milcah).
female(rebecca).
female(leah).
female(rachel).
female(bilhah).
female(zilpah).
female(dinah).

/**
@form parent(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the parent of person with Name2.
*/
parent(terah, abraham).
parent(terah, sarah).
parent(terah, haran).
parent(terah, nahor).
parent(nahor, bethuel).
parent(haran, lot).
parent(haran, milcah).
parent(haran, iscah).
parent(milcah, bethuel).
parent(abraham, isaac).
parent(abraham, ishmael).
parent(bethuel,rebecca).
parent(bethuel,laban).
parent(isaac, jacob).
parent(isaac, esau).
parent(laban, rachel).
parent(laban, leah).
parent(sarah, isaac).
parent(hagar, ishmael).
parent(rebecca, jacob).
parent(rebecca, esau).
parent(jacob, dinah).
parent(jacob, reuben).
parent(jacob, simeon).
parent(jacob, levi).
parent(jacob, judah).
parent(jacob, dan).
parent(jacob, naphtali).
parent(jacob, gad).
parent(jacob, asher).
parent(jacob, issachar).
parent(jacob, zebulun).
parent(jacob, joseph).
parent(jacob, benjamin).
parent(leah, dinah).
parent(leah, reuben).
parent(leah, simeon).
parent(leah, levi).
parent(leah, judah).
parent(bilhah, dan).
parent(bilhah, naphtali).
parent(zilpah, gad).
parent(zilpah, asher).
parent(leah, issachar).
parent(leah, zebulun).
parent(rachel, joseph).
parent(rachel, benjamin).

/**
@form mother(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the mother of person with Name2.
*/
mother(X,Y):- female(X), parent(X,Y).
/**
@form father(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the father of person with Name2.
*/
father(X,Y):- male(X), parent(X,Y).

/**
@form son(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the son of person with Name2.
*/
son(X,Y):- male(X), parent(Y,X).

/**
@form daughter(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is the daughter of person with Name2.
*/
daughter(X,Y):- female(X), parent(Y,X).

/**
@form spouse(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a spouse (has a child with)  of person with Name2.
*/
spouse(X,Y):- parent(X,Z), parent(Y,Z), not(X=Y).

/**
@form siblings(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a siblings of person with Name2.
*/
siblings(X,Y):- parent(Z,X), parent(Z,Y), not(X=Y).

/**
@form brother(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a brother of person with Name2.
*/
brother(X,Y):- male(X), siblings(X,Y).

/**
@form sister(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a sister of person with Name2.
*/
sister(X,Y):- female(X), siblings(X,Y).

/**
@form grandfather(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a grandparent of person with Name2.
*/
grandfather(X,Y):- male(X), parent(X,Z), parent(Z,Y).

/**
@form grandmother(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a grandmother of person with Name2.
*/
grandmother(X,Y):- female(X), parent(X,Z), parent(Y,Z).

/**
@form uncle(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is an uncle of person with Name2. Uncle is a male family relationship or kinship within an extended or immediate family. An uncle is the brother, half-brother, step-brother, or brother-in-law of one's parent, or the husband of one's aunt.
*/
uncle(X,Y):- parent(Z,Y), brother(X,Z), !.
uncle(X,Y):- spouse(X,Z), parent(K, Y), sister(Z,K).

aunt(X,Y):- parent(Z,Y), sister(X,Z), !.
aunt(X,Y):- spouse(X,Z), parent(K, Y), brother(Z,K).
/**
@form nephew(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a newphew of person with Name2.
*/
nephew(X,Y):- male(X), uncle(Y,X).
nephew(X,Y):- male(X), aunt(Y,X).

nephew(X,Y):- female(X), uncle(Y,X).
nephew(X,Y):- female(X), aunt(Y,X).
/**
@form cousin(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a cousin of person with Name2.
*/
cousin(X,Y):- grandfather(Z,X), grandfather(Z,Y), !.
cousin(X,Y):- grandmother(Z,X), grandmother(Z,Y).
/**
@form succesor(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a succesor of person with Name2.
*/
successor(X,Y):- ancestor(Y,X).
/**
@form ancestor(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is an ancestor of person with Name2.
*/
ancestor(X,Y):- parent(X,Y).
ancestor(X,Y):- parent(X,Z), ancestor(Z,Y).
/**
@form familty(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is family (has any conaction of family tree) of person with Name2.
*/
family(X,Y):- ancestor(Z,X), ancestor(Z,Y), not(X=Y).
family(X,Y):- ancestor(Z,X), parent(Y,K), ancestor(Z,K).
family(X,Y):- ancestor(Z,Y), parent(X,L), ancestor(Z,L).
family(X,Y):- successor(X,L), successor(Y,K), ancestor(Z,L), ancestor(Z,K).
/**
@form bad_spouse(Name1, Name2).
@constraints
  @unrestricted Name1.
  @unrestricted Name2.
@descr Person with Name1 is a spouse of person with Name2 and they have the same ancestor.
*/
bad_spouse(X,Y):- spouse(X,Y), ancestor(Z,X), ancestor(Z,Y).
