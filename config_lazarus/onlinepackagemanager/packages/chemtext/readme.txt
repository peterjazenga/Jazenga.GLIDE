--------------------------------------------------------------------------------
CHEMTEXT
--------------------------------------------------------------------------------

This library is intended to display chemical formulas and equations with 
automatically placed subscripts and superscripts. 

The procedure ChemTextOut draws the chemical text onto any canvas object 
(useful for writing event handlers for user drawn lists and grids). 
This procedure is used by the component TChemLabel to display chemical formulas 
on the forms.

The formulas are written in a straightforward way. The "2" in H2O is automatically
subscripted, and the + in H+ is automatically displayed as superscript. Note
that multiple-charge ions must repeat the charge sign, i.e. the double-negatively
charged oxygon ion must be written as O--, not as O2-.

For chemical reactions, arrows can be entered as -->, <-- or <-->. The property
Arrow of the TChemLabel determines whether this simple is to be replaced by
a nice UTF8 character.

The code is based on "chemtxt" written by Patrick Spanel 
(Patrik.Spanel@jh-inst.cas.cz). It was adapted to Lazarus and extended by 
Werner Pamler.

The code does not work under Delphi any more.

License:
LGPL with linking exception (the same license used by Lazarus).
