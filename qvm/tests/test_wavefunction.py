from program import run

def test_bell_states():
    state_prep = {
        "B00" : "",
        "B01" : "X 1\n",
        "B10" : "X 0\n",
        "B11" : "X 0\nX 1\n",
    }
    epr_program = "H 0\nCNOT 0 1"

    print('test')
    for i in state_prep:
        bell_program = state_prep[i] + epr_program

        run(bell_program)

    assert 1 == 0
