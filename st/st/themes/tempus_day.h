/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {
    /* 8 normal colors */
    [0] = "#3b2820",
    [1] = "#d62500",
    [2] = "#207f20",
    [3] = "#8a6900",
    [4] = "#306ad0",
    [5] = "#c6365d",
    [6] = "#00798d",
    [7] = "#f5f0f5",

    /* 8 bright colors */
    [8] = "#5b4440", 
    [9] = "#bb4b00", 
    [10] = "#5a7800",
    [11] = "#9a6033",
    [12] = "#6061d0",
    [13] = "#8850d0",
    [14] = "#007c6d",
    [15] = "#f8f1e0",

    /* more colors can be added after 255 to use with DefaultXX */

    /*
     * PRO TIP: this is good when trying to dynamically change from a
     * dark to a light theme or vice versa using ANSI escape sequences.
     *
     * Scenario:
     *
     * If on a dark theme we set defaultbg = 0 (with 15 being the
     * defaultfg), then changing the value of '0' to what would be
     * appropriate for a light theme (e.g. #ffffff) would turn the
     * background white, when what we really wanted was to alter the
     * foreground.
     * That is because the assumption for the design of the Tempus
     * themes is that colour codes represent colours.  So if a light
     * theme needs a black foregound, it must use color0.  Whereas
     * others default to defining the value of color0 as equivalent to
     * background regardless of the colour value (and color15 as the
     * foreground in the same way).  I think this is semantically
     * incorrect and might cause problems.
     *
     * For working examples, see my dotfiles, specifically the
     * 'own_script_update_running_terminals' under the "bin" directory:
     * https://gitlab.com/protesilaos/dotfiles.  Note that this script
     * is invoked from the 'own_script_update_environment_theme', which
     * is typically interfaced via the 'tempusmenu'.
     */
    [256] = "#f8f1e0",
    [257] = "#3b2820",
    [258] = "#3b2820",
    [259] = "#f8f1e0",
};

/* Default colors (colorname index)
 * foreground, background, cursor, reverse cursor
 */
unsigned int defaultbg = 256;
unsigned int defaultfg = 257;
static unsigned int defaultcs = 258;
static unsigned int defaultrcs = 259;
static unsigned int mousebg = 258;
static unsigned int mousefg = 259;
