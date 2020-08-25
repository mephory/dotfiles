#include "action.h"
#include "actionmap.h"
#include "hook.h"
#include "fc660c.h"

#define MACRO_TMUX_LAST_SESSION   1
#define MACRO_TMUX_NEXT_WINDOW 2
#define MACRO_TMUX_PREV_WINDOW 3
#define MACRO_TMUX_LAST_WINDOW 4
#define MACRO_TMUX_CREATE_WINDOW 5
#define MACRO_TMUX_SESSION_0 6
#define MACRO_TMUX_SESSION_1 7
#define MACRO_TMUX_SESSION_2 8
#define MACRO_TMUX_SESSION_3 9
#define MACRO_TMUX_SESSION_4 10
#define MACRO_TMUX_SESSION_5 11
#define MACRO_TMUX_WINDOW_1 12
#define MACRO_TMUX_WINDOW_2 13
#define MACRO_TMUX_WINDOW_3 14
#define MACRO_TMUX_WINDOW_4 15
#define MACRO_TMUX_WINDOW_5 16
#define MACRO_TMUX_WINDOW_6 17
#define MACRO_TMUX_WINDOW_7 18
#define MACRO_TMUX_WINDOW_8 19
#define MACRO_TMUX_WINDOW_9 20

#define AC_L1       ACTION_LAYER_MOMENTARY(1)
#define AC_LS_2     ACTION_LAYER_MODS(2, MOD_LSFT)
#define AC_L3       ACTION_LAYER_TAP_KEY(3, TAB)
#define AC_M_TM_LS  ACTION_MACRO(MACRO_TMUX_LAST_SESSION)
#define AC_M_TM_NW  ACTION_MACRO(MACRO_TMUX_NEXT_WINDOW)
#define AC_M_TM_PW  ACTION_MACRO(MACRO_TMUX_PREV_WINDOW)
#define AC_M_TM_LW  ACTION_MACRO(MACRO_TMUX_LAST_WINDOW)
#define AC_M_TM_CW  ACTION_MACRO(MACRO_TMUX_CREATE_WINDOW)
#define AC_M_TM_S0  ACTION_MACRO(MACRO_TMUX_SESSION_0)
#define AC_M_TM_S1  ACTION_MACRO(MACRO_TMUX_SESSION_1)
#define AC_M_TM_S2  ACTION_MACRO(MACRO_TMUX_SESSION_2)
#define AC_M_TM_S3  ACTION_MACRO(MACRO_TMUX_SESSION_3)
#define AC_M_TM_S4  ACTION_MACRO(MACRO_TMUX_SESSION_4)
#define AC_M_TM_S5  ACTION_MACRO(MACRO_TMUX_SESSION_5)
#define AC_M_TM_W1  ACTION_MACRO(MACRO_TMUX_WINDOW_1)
#define AC_M_TM_W2  ACTION_MACRO(MACRO_TMUX_WINDOW_2)
#define AC_M_TM_W3  ACTION_MACRO(MACRO_TMUX_WINDOW_3)
#define AC_M_TM_W4  ACTION_MACRO(MACRO_TMUX_WINDOW_4)
#define AC_M_TM_W5  ACTION_MACRO(MACRO_TMUX_WINDOW_5)
#define AC_M_TM_W6  ACTION_MACRO(MACRO_TMUX_WINDOW_6)
#define AC_M_TM_W7  ACTION_MACRO(MACRO_TMUX_WINDOW_7)
#define AC_M_TM_W8  ACTION_MACRO(MACRO_TMUX_WINDOW_8)
#define AC_M_TM_W9  ACTION_MACRO(MACRO_TMUX_WINDOW_9)

#ifdef KEYMAP_SECTION_ENABLE
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] __attribute__ ((section (".keymap.keymaps"))) = {
#else
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] PROGMEM = {
#endif
    [0] = KMAP(
        ESC, 1,   2,   3,   4,   5,   6,   7,   8,   9,   0,   MINS,EQL, BSPC,     INS,
        L3,  Q,   W,   E,   R,   T,   Y,   U,   I,   O,   P,   LBRC,RBRC,BSLS,     DEL,
        L1,  A,   S,   D,   F,   G,   H,   J,   K,   L,   SCLN,QUOT,     ENT,
        LS_2,Z,   X,   C,   V,   B,   N,   M,   COMM,DOT, SLSH,          RSFT,UP,
        LCTL,LGUI,LALT,          SPC,                     RALT,RCTL,L1, LEFT,DOWN,RGHT
    ),
    [1] = KMAP(
        GRV,   F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12,TRNS,     TRNS,
        TRNS,M_TM_S0,M_TM_S1,M_TM_S2,M_TM_S3,M_TM_S4,M_TM_S5,PGUP,HOME,PGDN,PSCR,TRNS,TRNS,TRNS,     TRNS,
        TRNS,M_TM_LW,M_TM_PW,M_TM_NW,MS_BTN1,M_TM_LS,LEFT,DOWN,UP,RIGHT,INSERT,DELETE,     TRNS,
        TRNS,TRNS,TRNS,M_TM_CW,TRNS,TRNS,END,ACL0,ACL1,ACL2,TRNS,          TRNS,TRNS,
        TRNS,TRNS,TRNS,          ACL0,                    TRNS,TRNS,TRNS,TRNS,TRNS,TRNS
    ),
    [2] = KMAP(
        GRV, TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,          TRNS,TRNS,
        TRNS,TRNS,TRNS,          TRNS,                    TRNS,TRNS,TRNS,TRNS,TRNS,TRNS
    ),
    [3] = KMAP(
        TRNS,M_TM_W1,M_TM_W2,M_TM_W3,M_TM_W4,M_TM_W5,M_TM_W6,M_TM_W7,M_TM_W8,M_TM_W9,TRNS,TRNS,TRNS,TRNS,     TRNS,
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,          TRNS,TRNS,
        TRNS,TRNS,TRNS,          TRNS,                    TRNS,TRNS,TRNS,TRNS,TRNS,TRNS
    ),
};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {
    if (record->event.pressed) {
        switch(id) {
        case MACRO_TMUX_LAST_SESSION:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), D(LSFT), T(L), U(LSFT), END);
        case MACRO_TMUX_PREV_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(P), END);
        case MACRO_TMUX_NEXT_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(N), END);
        case MACRO_TMUX_LAST_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), T(A), U(LCTL), END);
        case MACRO_TMUX_CREATE_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(C), END);
        case MACRO_TMUX_SESSION_0:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(F1) END);
        case MACRO_TMUX_SESSION_1:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(F2) END);
        case MACRO_TMUX_SESSION_2:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(F3) END);
        case MACRO_TMUX_SESSION_3:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(F4) END);
        case MACRO_TMUX_SESSION_4:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(F5) END);
        case MACRO_TMUX_SESSION_5:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(F6) END);
        case MACRO_TMUX_WINDOW_1:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(1), END);
        case MACRO_TMUX_WINDOW_2:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(2), END);
        case MACRO_TMUX_WINDOW_3:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(3), END);
        case MACRO_TMUX_WINDOW_4:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(4), END);
        case MACRO_TMUX_WINDOW_5:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(5), END);
        case MACRO_TMUX_WINDOW_6:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(6), END);
        case MACRO_TMUX_WINDOW_7:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(7), END);
        case MACRO_TMUX_WINDOW_8:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(8), END);
        case MACRO_TMUX_WINDOW_9:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(9), END);
        }
    }
    return MACRO_NONE;
    if (id == 1 && record->event.pressed) {
    } else if (id == 2 && record->event.pressed) {
    } else {
        return MACRO_NONE;
    }
}
