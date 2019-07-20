"""
Requirements Installation:
                                $ sudo pip3 install bottle
Usage:
        To run the server:
                                $ python3 server.py
        To open the web-page:
                                http://127.0.0.1/5000
        To teleport:
                                $ python3 teleport.py <qasm program file_name>
                                refresh the web page
"""
import os
from bottle import route, template, run, request


class global_values:
    value = "Nothing Teleported Yet!"
    index_page = """<html>
                        <body>
                            <h1>
                                Teleportation:
                            </h1>
                            {{value}}
                        </body>
                    </html>"""


@route('/', method='GET')
def get_index():
    return template(global_values.index_page, value=global_values.value)


@route('/', method='POST')
def teleport():
    q = [request.POST.get('q0'), request.POST.get('q1')]
    if len(q) == 2 and None not in q:
        global_values.value = "\n".join(q)
    else:
        global_values.value = "Nothing Teleported Yet!"


if os.environ.get('APP_LOCATION') == 'heroku':
    run(host="0.0.0.0", port=int(os.environ.get("PORT", 5000)))
else:
    run(host='localhost', port=5000, debug=True, reloader=True)
