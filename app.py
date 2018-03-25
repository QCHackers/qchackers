from flask import Flask, request, jsonify, render_template
from datetime import datetime
from qvm import vm
import program

app = Flask(__name__)

@app.route('/')
def homepage():
    the_time = datetime.now().strftime("%A, %d %b %Y %l:%M %p")

    return """
    <h1>Hello heroku</h1>
    <p>It is currently {time}.</p>
    <img src="http://loremflickr.com/600/400" />
    """.format(time=the_time)


@app.route('/tic-tac-toe')
def homepage():
    return render_template('index.html')


@app.route('/api/add_message/<uuid>', methods=['GET', 'POST'])
def add_message(uuid):
    content = request.json
    print (content['mytext'])
    p = content['mytext']
    wvf, msg = program.run(p)
    return jsonify({"results" : msg})




@app.route('/move', methods=['POST'])
def move():
    post = request.get_json()

    game = Game()
    game.board = post.get('board')
    game.player = post.get('player')
    game.computer = post.get('computer')

    # Check if player won
    if game.has_won(game.player):
        return jsonify(tied = False, computer_wins = False, player_wins = True, board = game.board)
    elif game.is_board_full():
        return jsonify(tied = True, computer_wins = False, player_wins = False, board = game.board)

    # Calculate computer move
    computer_move = game.calculate_move()

    # Make the next move
    game.make_computer_move(computer_move['row'], computer_move['col'])

    # Check if computer won
    if game.has_won(game.computer):
        return jsonify(computer_row = computer_move['row'], computer_col = computer_move['col'],
                       computer_wins = True, player_wins = False, tied=False, board = game.board)
    # Check if game is over
    elif game.is_board_full():
        return jsonify(computer_row = computer_move['row'], computer_col = computer_move['col'],
                       computer_wins = False, player_wins = False, tied=True, board=game.board)

    # Game still going
    return jsonify(computer_row = computer_move['row'], computer_col = computer_move['col'],
                   computer_wins = False, player_wins = False, board = game.board)

if __name__ == '__main__':
    app.run(debug=True, use_reloader=True)
