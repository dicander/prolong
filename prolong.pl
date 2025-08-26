% Pong - Scryer Prolog + raylib (NO AUDIO, via wrapper — no structs)
% Left: human (W/S). Right: AI (alternates: simple, smart) every new ball.
% ESC or close window to quit.

% pointless change
:- use_module(library(ffi)).
:- use_module(library(random)).

:- initialization(main).

main :-
    use_foreign_module("./librlwrap.so", [
        'RL_InitWindow'([sint32,sint32,cstr], void),
        'RL_SetTargetFPS'([sint32], void),
        'RL_WindowShouldClose'([], sint32),
        'RL_IsKeyDown'([sint32], sint32),
        'RL_BeginDrawing'([], void),
        'RL_EndDrawing'([], void),
        % RGBA as sint32 (avoid uint8 on this Scryer build)
        'RL_ClearBackgroundRGBA'([sint32,sint32,sint32,sint32], void),
        'RL_DrawRectangleRGBA'([sint32,sint32,sint32,sint32,sint32,sint32,sint32,sint32], void),
        'RL_CloseWindow'([], void)
    ]),

    % Window
    W = 960, H = 540,
    ffi:'RL_InitWindow'(W, H, "Prolong"),
    ffi:'RL_SetTargetFPS'(60),

    % Initial state
    PW = 12, PH = 90, Ball = 8,
    PSpeed = 420.0, BSpeed = 360.0, Dt is 1.0/60.0,
    X0 is W/2, Y0 is H/2, Vx0 = BSpeed, Vy0 is 0.65*BSpeed,
    LPY0 is H/2, RPY0 is H/2, LS0 = 0, RS0 = 0,
    AI0 = simple,   % start with simple, will toggle on each new serve
    S0 = state(X0,Y0,Vx0,Vy0,LPY0,RPY0,LS0,RS0,AI0),

    loop(W,H,PW,PH,Ball,PSpeed,Dt,S0),

    ffi:'RL_CloseWindow'.

% ---------------- Loop & input ----------------

loop(W,H,PW,PH,Ball,PSpeed,Dt,S0) :-
    ffi:'RL_WindowShouldClose'(C),
    (  C =\= 0
    -> true
    ;  step(W,H,PW,PH,Ball,PSpeed,Dt,S0,S1),
       draw(W,H,PW,PH,Ball,S1),
       loop(W,H,PW,PH,Ball,PSpeed,Dt,S1)
    ).

is_key_down(Key) :- ffi:'RL_IsKeyDown'(Key, R), R =\= 0.

% ---------------- Update ----------------

step(W,H,PW,PH,Ball,PSpeed,Dt,
     state(X,Y,Vx,Vy,LPY,RPY,LS,RS,AI),
     state(X2,Y2,Vx2,Vy2,LPY2,RPY2,LS2,RS2,AI2)) :-

    key_w(KW), key_s(KS),

    % Left paddle: human W/S
    ( is_key_down(KW) -> LPYa is LPY - PSpeed*Dt ; LPYa = LPY ),
    ( is_key_down(KS) -> LPYb is LPYa + PSpeed*Dt ; LPYb = LPYa ),
    clamp_paddle(H,PH,LPYb,LPY2),

    % Ball integrate
    X1 is X + Vx*Dt, Y1 is Y + Vy*Dt,

    % Top/bottom bounce
    ( Y1 - Ball < 0.0  -> Yw is Ball,       Vyw is -Vy
    ; Y1 + Ball > H    -> Yw is H - Ball,   Vyw is -Vy
    ;                      Yw = Y1,         Vyw = Vy
    ),

    % Paddle x and y (top)
    Lx is 24, Rx is W - 24 - PW,
    Ly1 is LPY2 - PH/2,

    % Right paddle target from AI, move at most PSpeed*Dt
    r_ai(AI, W,H,PW,PH,Ball,PSpeed,Dt, Rx, X,Y,Vx,Vyw, LPY2, RPY, RPYaim),
    MaxStep is PSpeed*Dt,
    move_towards(RPY, RPYaim, MaxStep, RPYmove),
    clamp_paddle(H,PH,RPYmove,RPYmClamped),
    RPYc = RPYmClamped,
    Ry1 is RPYc - PH/2,

    % Left paddle collision
    ( X1 - Ball =< Lx + PW, Yw >= Ly1, Yw =< Ly1 + PH, Vx < 0 ->
        Xc is Lx + PW + Ball,
        Vxc is -Vx * 1.02,
        Angle is (Yw - LPY2)/(PH/2),
        Vyc is Vyw + Angle*120.0
    ;   Xc = X1, Vxc = Vx, Vyc = Vyw
    ),

    % Right paddle collision
    ( Xc + Ball >= Rx, Yw >= Ry1, Yw =< Ry1 + PH, Vxc > 0 ->
        Xr is Rx - Ball,
        Vxr is -Vxc * 1.02,
        Angle2 is (Yw - RPYc)/(PH/2),
        Vyr is Vyc + Angle2*120.0
    ;   Xr = Xc, Vxr = Vxc, Vyr = Vyc
    ),

    % Scoring & serve (toggle AI every new ball)
    ( Xr < -40.0 ->
        RS2 is RS + 1, LS2 = LS,
        reset_ball(W,H,BVx,BVy,X2,Y2), Vx2 = BVx,  Vy2 = BVy,
        toggle_ai(AI, AI2),
        RPY2 = RPYc
    ; Xr > W + 40.0 ->
        LS2 is LS + 1, RS2 = RS,
        reset_ball(W,H,BVx2,BVy2,X2,Y2), Vx2 = BVx2, Vy2 = BVy2,
        toggle_ai(AI, AI2),
        RPY2 = RPYc
    ;   X2 = Xr, Y2 = Yw, Vx2 = Vxr, Vy2 = Vyr, LS2 = LS, RS2 = RS,
        AI2 = AI, RPY2 = RPYc
    ).

clamp_paddle(H,PH,In,Out) :-
    Half is PH/2, Min is Half, Max is H - Half,
    ( In < Min -> Out = Min
    ; In > Max -> Out = Max
    ;            Out = In
    ).

move_towards(Curr, Target, MaxStep, Out) :-
    D is Target - Curr,
    ( D >  MaxStep -> Out is Curr + MaxStep
    ; D < -MaxStep -> Out is Curr - MaxStep
    ;                Out = Target
    ).

toggle_ai(simple, smart).
toggle_ai(smart,  simple).

% --- AI controllers (right paddle) ---

% r_ai(+AI, +W,+H,+PW,+PH,+Ball,+PSpeed,+Dt, +Rx, +X,+Y,+Vx,+Vy, +LPY, +RPY, -RPYtarget)
r_ai(simple, _W,_H,_PW,_PH,_Ball,_PSpeed,_Dt, _Rx, _X,Y,_Vx,_Vy, _LPY, _RPY, TargetY) :-
    TargetY = Y.

r_ai(smart, _W,H,_PW,_PH,Ball,_PSpeed,_Dt, Rx, X,Y,Vx,Vy, _LPY, _RPY, TargetY) :-
    ( Vx =< 0.0 ->
        TargetY is H/2
    ;   predict_y_at_x(X,Y,Vx,Vy, Rx - Ball, H, Ball, TargetY)
    ).

% Predict Y when the ball (center) reaches target X, reflecting on top/bottom.
predict_y_at_x(X,Y,Vx,Vy, Xtarget, H, Ball, Yout) :-
    T is (Xtarget - X) / Vx,
    ( T =< 0.0 -> Yout = Y
    ; predict_y_after_time(Y,Vy,T,H,Ball,Yout)
    ).

% Reflective vertical motion for time T with walls at [Ball, H-Ball]
predict_y_after_time(Y,_Vy,T, _H,_Ball, Y) :- T =< 0.0, !.
predict_y_after_time(Y,0.0,_T, _H,_Ball, Y) :- !.
predict_y_after_time(Y,Vy,T, H,Ball, Yout) :-
    Top is Ball, Bot is H - Ball,
    ( Vy > 0.0 ->
        DtWall is (Bot - Y)/Vy
    ; DtWall is (Y - Top)/(-Vy)
    ),
    ( DtWall =< 0.0 ->
        % already “past” numerically: snap to nearest wall, flip and continue
        ( Vy > 0.0 -> Y1 = Bot ; Y1 = Top ),
        Vy1 is -Vy,
        predict_y_after_time(Y1,Vy1,T,H,Ball,Yout)
    ; T =< DtWall ->
        Yout is Y + Vy*T
    ; % bounce, reflect velocity and continue
      ( Vy > 0.0 -> Y1 = Bot ; Y1 = Top ),
      Vy1 is -Vy,
      T1 is T - DtWall,
      predict_y_after_time(Y1,Vy1,T1,H,Ball,Yout)
    ).

reset_ball(W,H,Vx,Vy,X,Y) :-
    X is W/2, Y is H/2,
    BSpeed = 360.0,
    random(R1), (R1 < 0.5 -> Sx = -1 ; Sx = 1),
    random(R2), Ry is 0.25 + 0.60*R2,
    random(R3), (R3 < 0.5 -> Sy = -1 ; Sy = 1),
    Vx is Sx * BSpeed,
    Vy is Sy * BSpeed * Ry.

% ---------------- Draw ----------------

draw(W,_H,PW,PH,Ball,state(X,Y,_,_,LPY,RPY,LS,RS,_AI)) :-
    % precompute ints for the FFI
    Lx is 24, Ly is LPY - PH/2,
    Rx is W - 24 - PW, Ry is RPY - PH/2,
    LyI is round(Ly), RyI is round(Ry),
    Bx is round(X) - Ball, By is round(Y) - Ball, Sz is Ball*2,
    LeftScoreX  is 80, RightScoreX is W - 80, ScoreY is 40,
    ffi:'RL_BeginDrawing',
      ffi:'RL_ClearBackgroundRGBA'(0,0,0,255),
      ffi:'RL_DrawRectangleRGBA'(Lx, LyI, PW, PH, 255,0,0,255),
      ffi:'RL_DrawRectangleRGBA'(Rx, RyI, PW, PH, 0,255,0,255),
      ffi:'RL_DrawRectangleRGBA'(Bx, By, Sz, Sz, 255,255,255,255),
      draw_bars(LeftScoreX,  ScoreY, LS),
      draw_bars(RightScoreX, ScoreY, RS)
    ,
    ffi:'RL_EndDrawing'.

draw_bars(_,_,N) :- N =< 0, !.
draw_bars(X,Y,N) :-
    XBar is X - 10,
    YTop is Y,
    YGap is Y + 6,
    ffi:'RL_DrawRectangleRGBA'(XBar, YTop, 20, 6, 0,0,255,255),
    ffi:'RL_DrawRectangleRGBA'(XBar, YGap, 20, 2, 0,0,180,255),
    N1 is N - 1,
    YNext is Y + 12,
    draw_bars(X, YNext, N1).

key_w(87). key_s(83). key_up(265). key_down(264).
