TRingChart component is an elliptic Sectors Chart that we can consider a twin of the LCL TChart Component in the sense that has the same structure, particularly for the data input structure, but the rest is quite original.
What's new?
1. it is accurate in the sense that every sector surface is really proportionated to his pie percentage (TCanvas.RadialPie is similar but not exact for area) so if a green sector value is 17.27% of total values the green area is the 17.27% of the total ellipse area;
2. it allow to include:
- label (value and/or percentage)
- legend (none, inside or outside)
3. it manage value as double (not only integer)
4. it allows:
- change a single sector (Name or label, Value and color)
- delete a single sector;
5. it can appeares as a ring sectorized or a Pie

TAnalogWatch is a graphic component that use a timer to show the current time in analog way. Just put it on a form.

The 3D appearance (thickness) is quite rough so should be improved.

An OnAppointment notify event has been added so at a particular time (MeetTime) this event is triggered (in the example it shown a "coocoo" message).
Note that MeetTime is a string and a simple property editor is provided for set it in a correct way.
Note that if you want to set very important appointment you should consider to add a furthermore check (i.e. if the MeetTime has been surpassed and the event has not been triggered due to system fails like power down, ontimer delayed and so on, the user shoul be notified or the onAppointment should be retriggered)

Enhancments from Seth Grover:
1. mode
"Clock" mode - the current default mode
"Stopwatch" mode - behaves like a stopwatch (you set the mode to stopwatch mode and then set "StopwatchRunning" to true or false to start/stop the stopwatch
"Defined" mode - simply displays the time you tell it to display with the DefinedTime property
2. Added a couple more properties to enable/disable displaying of things like the tick marks, the digits on the clock face, and each hand individually (so you could only show the minute and hour hand, or just the second hand, etc.).


Note that some Ellipse procedures I written come in a separate file named ellipse.pas.




How to use them: see the example and click on "Random Values"

Note that this components are distribuite under the same licence of the Lazarus LCL (see COPYING.LCL in this distribution or in the Lazarus distribution)