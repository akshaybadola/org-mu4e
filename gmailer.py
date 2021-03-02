import os
import shlex
import json
import configargparse
import argparse
from subprocess import Popen, PIPE

from flask import Flask, request
from werkzeug import serving

import base64
from oauth2client.client import OAuth2Credentials
from googleapiclient import discovery


def encode(v):
    byte_msg = v.encode(encoding="UTF-8")
    byte_msg_b64encoded = base64.b64encode(byte_msg)
    return byte_msg_b64encoded.decode(encoding="UTF-8")


class Mailer:
    def __init__(self, port, users):
        self.port = port
        self.service = {}
        self.creds = {}
        self.users = users
        if not users:
            raise AttributeError("Has to be at least one user")
        print(f"Running for users: {self.users}")
        for user in self.users:
            self.creds[user] = self.read_creds(user)
            if self.creds[user] is not None:
                self.service[user] = discovery.build('gmail', 'v1', credentials=self.creds[user])
            else:
                self.service[user] = None
        self.app = Flask("gmail server")

    def read_creds(self, user):
        try:
            p = Popen(shlex.split(f"pass mail/{user}"), stdout=PIPE)
            out, err = p.communicate()
            return OAuth2Credentials.from_json(out)
        except Exception:
            print(f"Could not read credentials for user {user}")
            return None

    def send_message(self, user: str, message: str):
        return self.service[user].users().\
            messages().send(userId="me", body={"raw": encode(message)}).execute()

    def run(self):
        @self.app.route("/sendmail", methods=["GET"])
        def __sendmail():
            try:
                user = request.args.get("user")
                filename = request.args.get("filename")
            except Exception:
                return "Error. No user or filename in data"
            with open(os.path.join(os.path.expanduser("~/" + filename))) as f:
                msg = f.read()
            if user not in self.users:
                return f"Error. Unknown User {user}"
            else:
                return "Success. " + json.dumps(self.send_message(user, msg))

        serving.run_simple("localhost", self.port, self.app, threaded=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--users", "-u", type=str,
                        default=",".join(["jaali.account@gmail.com",
                                          "akshay.badola.cs@gmail.com",
                                          "badola@uohyd.ac.in",
                                          "atavist13@gmail.com"]),
                        help="Comma separated user names")
    parser.add_argument("--port", "-p", type=int, default=2233,
                        help="port on which to serve")
    args = parser.parse_args()
    port = args.port
    users = [x.strip() for x in args.users.split(",")]
    mailer = Mailer(port, users)
    mailer.run()
