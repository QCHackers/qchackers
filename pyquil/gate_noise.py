from pyquil.noise import add_decoherence_noise
records = []
for theta in thetas:
    for t1 in t1s:
        prog = get_compiled_prog(theta)
        noisy = add_decoherence_noise(prog, T1=t1).inst([
            MEASURE(0, 0),
            MEASURE(1, 1),
        ])
        bitstrings = np.array(cxn.run(noisy, [0, 1], 1000))
