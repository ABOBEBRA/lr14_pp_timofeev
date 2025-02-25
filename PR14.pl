% ���� ������ ��������� �����������
d(X, X, 1) :- !. % ����������� ���������� �� ����� ���� ����� 1
d(C, _X, 0) :- atomic(C). % ����������� ��������� ����� 0
d(-U, X, -A) :- d(U, X, A). % (-u)' = -u'
d(U+V, X, A+B) :- d(U, X, A), d(V, X, B). % (u+v)' = u' + v'
d(U-V, X, A-B) :- d(U, X, A), d(V, X, B). % (u-v)' = u' - v'
d(C*U, X, C*A) :- atomic(C), C \= X, d(U, X, A), !. % (c*u)' = c*u'
d(U*V, X, U*B + A*V) :- d(U, X, A), d(V, X, B). % (uv)' = uv' + u'v
d(U/V, X, (A*V - U*B)/V^2) :- d(U, X, A), d(V, X, B). % (u/v)' = (u'v - uv')/v^2
d(U^C, X, C*A*U^(C-1)) :- d(U, X, A). % (u^c)' = c*u^(c-1)*u'
d(log(U), X, A/U) :- d(U, X, A). % (log(u))' = u'/u

% �������������� ������� ��� ������� �������
d(exp(U), X, A*exp(U)) :- d(U, X, A). % (exp(u))' = u'*exp(u)
d(sin(U), X, A*cos(U)) :- d(U, X, A). % (sin(u))' = u'*cos(u)
d(cos(U), X, -A*sin(U)) :- d(U, X, A). % (cos(u))' = -u'*sin(u)

% ��������� ���������
���������(E, E) :- atomic(E), !. % ���� ��������� ��������, ��� ��� ��������
���������(E, R) :- E =.. [Op, A], ���������(A, X), s(Op, X, R). % ��������� ������� ����������
���������(E, R) :- E =.. [Op, A, B],
                   ���������(A, X), ���������(B, Y), s(Op, X, Y, R). % ��������� �������� ����������

% ������� ���������
s(+, X, Y, R) :- number(X), number(Y), R is X + Y. % 2+1=3
s(-, X, Y, R) :- number(X), number(Y), R is X - Y. % 2-1=1
s(*, X, Y, R) :- number(X), number(Y), R is X * Y. % 2*3=6
s(/, X, Y, R) :- number(X), number(Y), Y \= 0, R is X / Y. % 4/2=2

s(+, X, 0, X). % x+0=x
s(-, X, 0, X). % x-0=x
s(+, 0, X, X). % 0+x=x
s(-, 0, X, -X). % 0-x=-x
s(-, X, X, 0). % x-x=0
s(+, X*Z, Y*Z, R*Z) :- number(X), number(Y), R is X + Y. % x*z+y*z=(x+y)*z
s(-, X*Z, Y*Z, R*Z) :- number(X), number(Y), R is X - Y. % x*z-y*z=(x-y)*z

s(*, 0, _, 0). % 0*a=0
s(*, _, 0, 0). % a*0=0
s(*, X, 1, X). % x*1=x
s(*, 1, X, X). % 1*x=x
s(*, X, X, X^2). % x*x=x^2
s(*, -X, X, -X^2). % (-x)*x=-x^2
s(*, C*X, X, C*X^2) :- number(C). % c*x*x=c*x^2
s(*, C*X, X^N, C*X^K) :- number(C), K is N + 1. % c*x*x^n=c*x^(n+1)
s(*, X, X^N, X^K) :- K is N + 1. % x*x^n=x^(n+1)

s(^, 0, _, 0). % 0^a=0
s(^, _, 0, 1). % a^0=1
s(^, X, 1, X). % x^1=x

% ������� ��� ����������, ���� �� ������� ����������� �������
s(+, X, Y, X + Y). % x+y=x+y
s(-, X, Y, X - Y). % x-y=x-y
s(*, X, Y, X * Y). % x*y=x*y
s(/, X, Y, X / Y). % x/y=x/y
s(^, X, Y, X ^ Y). % x^y=x^y

% ����������� ����������� � ����������
derivative(F, X, Result) :-
    d(F, X, D),
    ���������(D, Result).

save_result_txt(File, Expression, Derivative) :-
    open(File, write, Stream),
    format(Stream, "Expression: ~w~n", [Expression]),
    format(Stream, "Derivative: ~w~n", [Derivative]),
    close(Stream).

save_result_mat(File, Expression, Derivative) :-
    open(File, write, Stream),
    format(Stream, 'expression = \'~w\';~n', [Expression]),
    format(Stream, 'derivative = \'~w\';~n', [Derivative]),
    close(Stream).

save_result_custom(File, Expression, Derivative) :-
    (   open(File, write, Stream, []) -> % ��������� ���������� �������� �����
        (
            format(Stream, '=== Differential Calculation ===~n', []),
            format(Stream, 'Expression: ~w~n', [Expression]),
            format(Stream, 'Derivative: ~w~n', [Derivative]),
            format(Stream, '==============================~n', []),
            close(Stream) % ��������� ����� ����� ������
        )
    ;   write('������ ��� �������� �����.'), nl % ��������� ������ �������� �����
    ).

save_result_html(File, Expression, Derivative) :-
    open(File, write, Stream),
    format(Stream, '<!DOCTYPE html><html><body>', []),
    format(Stream, '<h1>�����������������</h1>', []),
    format(Stream, '<p>�������: ~w</p>', [Expression]),
    format(Stream, '<p>�����������: ~w</p>', [Derivative]),
    format(Stream, '</body></html>', []),
    close(Stream).


% ������� ��� ���������� ����������� � ����
save_result(File, Format, Expression, Derivative) :-
    (   Format == txt -> save_result_txt(File, Expression, Derivative)
    ;   Format == mat -> save_result_mat(File, Expression, Derivative)
    ;   Format == custom -> save_result_custom(File, Expression, Derivative)
    ;   Format == html -> save_result_html(File, Expression, Derivative)
    ;   write('Unsupported format'), nl
    ).


% �������� ����� ����� � ���������� ��������
create_filename(BaseName, Format, FileName) :-
    atomic_list_concat([BaseName, '.', Format], FileName).

% ��������� ������������
main :-
    write('������� ������� ��� �����������������: '),
    read(F),
    write('������� ���������� �����������������: '),
    read(X),
    derivative(F, X, Result),
    write('�����������: '), writeln(Result),
    write('��������� ��������� � ����? (yes/no): '),
    read(Answer),
    (   Answer == yes ->
        write('������� ��� ����� (��� ����������): '),
        read(BaseName),
        write('�������� ������ (txt/mat/custom/html): '),
        read(Format),
        create_filename(BaseName, Format, FileName), % ������� ��� �����
        save_result(FileName, Format, F, Result),
        write('��������� �������.')
    ;   true
    ).
