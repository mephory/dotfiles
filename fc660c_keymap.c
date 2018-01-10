#include "action.h"
#include "actionmap.h"
#include "hook.h"
#include "fc660c.h"

#define MACRO_TMUX_LAST_SESSION   1
#define MACRO_TYPE_EMAIL 2
#define MACRO_TMUX_NEXT_WINDOW 3
#define MACRO_TMUX_PREV_WINDOW 4
#define MACRO_TMUX_LAST_WINDOW 5
#define MACRO_TMUX_CREATE_WINDOW 6

#define AC_L1       ACTION_LAYER_MOMENTARY(1)
#define AC_LS_2     ACTION_LAYER_MODS(2, MOD_LSFT)
#define AC_M_TM_LS  ACTION_MACRO(1)
#define AC_M2       ACTION_MACRO(2)
#define AC_M_TM_NW  ACTION_MACRO(3)
#define AC_M_TM_PW  ACTION_MACRO(4)
#define AC_M_TM_LW  ACTION_MACRO(5)
#define AC_M_TM_CW  ACTION_MACRO(6)

#ifdef KEYMAP_SECTION_ENABLE
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] __attribute__ ((section (".keymap.keymaps"))) = {
#else
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] PROGMEM = {
#endif
    [0] = KMAP(
        ESC, 1,   2,   3,   4,   5,   6,   7,   8,   9,   0,   MINS,EQL, BSPC,     INS,
        TAB, Q,   W,   E,   R,   T,   Y,   U,   I,   O,   P,   LBRC,RBRC,BSLS,     DEL,
        L1,  A,   S,   D,   F,   G,   H,   J,   K,   L,   SCLN,QUOT,     ENT,
        LS_2,Z,   X,   C,   V,   B,   N,   M,   COMM,DOT, SLSH,          RSFT,UP,
        LCTL,LGUI,LALT,          SPC,                     RALT,RCTL,L1, LEFT,DOWN,RGHT
    ),
    [1] = KMAP(
        GRV,   F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12,TRNS,     TRNS,
        TRNS,MS_BTN2,MS_UP,M2,TRNS,TRNS,TRNS,PGUP,HOME,PGDN,PSCR,TRNS,TRNS,TRNS,     TRNS,
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
        GRV,   F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12,TRNS,     TRNS,
        TRNS,MS_BTN2,MS_UP,M2,TRNS,TRNS,TRNS,PGUP,HOME,PGDN,PSCR,TRNS,TRNS,TRNS,     TRNS,
        TRNS,MS_LEFT,MS_DOWN,MS_RIGHT,MS_BTN1,M_TM_LS,LEFT,DOWN,UP,RIGHT,INSERT,DELETE,     TRNS,
        TRNS,TRNS,TRNS,ESC,TRNS,TRNS,END,ACL0,ACL1,ACL2,TRNS,          TRNS,TRNS,
        TRNS,TRNS,TRNS,          ACL0,                    TRNS,TRNS,TRNS,TRNS,TRNS,TRNS
    ),
};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {
    if (record->event.pressed) {
        switch(id) {
        case MACRO_TMUX_LAST_SESSION:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), D(LSFT), T(L), U(LSFT), END);
        case MACRO_TYPE_EMAIL:
            return MACRO(I(0), T(M), T(E), T(P), T(H), T(O), T(R), T(Y), D(LSFT), T(2), U(LSFT), T(M), T(E), T(P), T(H), T(O), T(R), T(Y), T(DOT), T(C), T(O), T(M), END);
        case MACRO_TMUX_PREV_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(P), END);
        case MACRO_TMUX_NEXT_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(N), END);
        case MACRO_TMUX_LAST_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), T(A), U(LCTL), END);
        case MACRO_TMUX_CREATE_WINDOW:
            return MACRO(I(0), D(LCTL), T(A), U(LCTL), T(C), END);
        }
    }
    return MACRO_NONE;
    if (id == 1 && record->event.pressed) {
    } else if (id == 2 && record->event.pressed) {
    } else {
        return MACRO_NONE;
    }
}
