# Summary of key bindings for my Xorg Tiling Window Managers

This documents pertains to `bspwm` and `herbstluftwm`.  It was written
on 2023-05-14.  Many of the commands called by these key bindings
invoke my custom shell scripts.

* * *

1. The `<motion>` refers to the keys `h`, `j`, `k`, `l` as well as the
   arrow keys.  The motions are for left, down, up, right,
   respectively.

2. The `<number>` refers to numbers `0` through `9`.

3. The "Super" key is the one that typically has the "Win" notation or
   the Windows logo.

4. Key bindings are expressed as a combination that includes the plus
   sign, such as `Super + d`.

5. Bindings that involve chains of key presses are known as "key
   chords".  These are expressed with the semicolon as a delimiter
   between distinct key presses.  So `Super + e ; f` means "hold down
   `Super` and press `e`; release `Super` and `e`, then press `f`".

## Common key bindings

| Key                               | Description                                                               |
|-----------------------------------+---------------------------------------------------------------------------|
| Super + F5                        | Reload the environemnt's theme (toggle between light and dark)            |
| Ctrl + Alt + Delete               | Bring up the "poweroptionsmenu" script to log out, restart, shutdown      |
| Super + Delete                    | Same as above                                                             |
| Super + d                         | Show a menu for desktop applications                                      |
| Super + b                         | Show a menu with open windows (the "b" is a mnemonic for "buffers")       |
| Super + Return                    | Start a new XTerm instance                                                |
| Super + Shift + Return            | Start a new emacsclient for the current Emacs server or create new server |
| Super + Ctrl + Shift + Return     | Like the above, but also evaluate the 'shell' command                     |
| Print (PrtSc)                     | Take a screenshot of the current window                                   |
| Super + Print                     | Take a screenshot of the entire screen                                    |
| Super + Shift + Print             | Activate screenshooter to capture the subsequent mouse-dragged region     |
| BrightnessUp (may involve Fn key) | Increase screen brightness if the hardware supports it                    |
| BrightnessDown                    | Same as above for a decrease                                              |
| VolumeUp (may involve Fn key)     | Increase volume of audio output                                           |
| VolumeDown                        | Decrease volume of audio output                                           |
| Super + VolumeUp                  | Increase volume of the microphone input                                   |
| Super + VolumeDown                | Decrease volume of the microphone input                                   |
| MediaNext (may involve Fn key)    | Play the next track in the last active MPRIS media player                 |
| MediaPrevious                     | Same as above for previous                                                |
| MediaPlay                         | Same as above for play/pause                                              |

## Environment key bindings

These apply to the "entire environment" and thus have the prefix
`Super + e ;`.

| Key                | Description                                                          |
|--------------------+----------------------------------------------------------------------|
| Super + e ; f      | Enter the "focus mode", which removes the panel and any gaps         |
| Super + e ; s      | Reload the SXHKD daemon, thus applying new key bindings              |
| Super + e ; p      | Reload the system panel, which is called "polybar"                   |
| Super + e ; x      | Reload the XResources files, which affects XTerm                     |
| Super + e ; t      | Reload the environemnt's theme (toggle between light and dark)       |
| Super + e ; Delete | Bring up the "poweroptionsmenu" script to log out, restart, shutdown |

## HerbstluftWM

| Key                                | Description                                                                 |
|------------------------------------+-----------------------------------------------------------------------------|
| Super + q                          | Close current window                                                        |
| Super + Shift + q                  | Close current window and remove its frame if possible                       |
| Super + Alt + q                    | Remove current frame                                                        |
| Super + <motion>                   | Move focus to the window in the given direction                             |
| Super + Shift + <motion>           | Shift/reposition the focused window in the given direction                  |
| Super + Ctrl + <motion>            | Resize current frame (not window!) in the given direction                   |
| Super + Alt + <motion>             | Split frame in the given direction                                          |
| Super + <number>                   | Move focus to the desktop whose name is the given number                    |
| Super + Shift + <number>           | Send the focused window to the desktop with the given number                |
| Super + f                          | Toggle window fullscreen view                                               |
| Super + Space                      | Toggle window floating/tiling state                                         |
| Super + Tab                        | Cycle forward to the next desktop                                           |
| Super + Shift + Tab                | As above, but backward                                                      |
| Alt + Tab                          | Switch focus to the next window in the current desktop                      |
| Alt + Shift + Tab                  | Same as above, in the opposite direction                                    |
| Alt + Backquote                    | Cycle forward to the next window in the current frame                       |
| Alt + Shift + Backquote            | As above, but backward                                                      |
| Super + r                          | Rotate frames in the current desktop                                        |
| Super + Shift + r                  | Mirror frames in the current desktop                                        |
| Super + t                          | Cycle between vertical, horizontal, and max (tabbed) current framed layouts |
| Super + RightSquareBracket         | Cycle window gaps from min to max and loop by increasing their size         |
| Super + LeftSquareBracket          | Same as above, but works by diminishing the gaps                            |
| Super + Shift + RightSquareBracket | Like the window gap cycling but for window borders                          |
| Super + Shift + LeftSquareBracket  | Same as above                                                               |

## BSPWM

This section is incomplete.  I omit some of the more obscure
operations.

There also are a few key bindings I no longer use.  They are from the
time I had a small laptop attached to an external monitor.

| Key                                | Description                                                                  |
|------------------------------------+------------------------------------------------------------------------------|
| Super + q                          | Close the focused window                                                     |
| Super + Shift + q                  | Forcefully close/kill the focused window                                     |
| Super + <motion>                   | Move focus to the window in the given direction                              |
| Super + Shift + <motion>           | Shift/reposition the focused window in the given direction                   |
| Super + <number>                   | Move focus to the desktop whose name is the given number                     |
| Super + Ctrl + <number>            | Move the other monitor's displayed yet unfocused desktop to the given number |
| Super + Shift + <number>           | Send the focused window to the desktop with the given number                 |
| Super + Ctrl + Shift + <number>    | Same as above, but also send the entire desktop to the next monitor          |
| Super + Tab                        | Move to the next desktop in the current monitor                              |
| Super + Shift + Tab                | Same as above, in the opposite direction                                     |
| Alt + Tab                          | Switch focus to the next window in the current desktop                       |
| Alt + Shift + Tab                  | Same as above, in the opposite direction                                     |
| Super + Ctrl + <motion>            | Expand or contract window in the given direction                             |
| Super + Alt + <motion>             | Preselect area in the given direction for placement of next created window   |
| Super + Alt + <number>             | Adjust the split ratio of the above                                          |
| Super + m                          | Toggle window monocle view (maximisation state)                              |
| Super + f                          | Toggle window fullscreen view                                                |
| Super + Space                      | Toggle window floating/tiling state                                          |
| Super + r                          | Rotate the windows in the current desktop 90 degrees clockwise               |
| Super + Shift + r                  | Same as above, counter-clockwise                                             |
| Super + RightSquareBracket         | Cycle window gaps from min to max and loop by increasing their size          |
| Super + LeftSquareBracket          | Same as above, but works by diminishing the gaps                             |
| Super + Shift + RightSquareBracket | Like the window gap cycling but for window borders                           |
| Super + Shift + LeftSquareBracket  | Same as above                                                                |
