#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import flask
import requests
import redis

import google.oauth2.credentials
import google_auth_oauthlib.flow
import googleapiclient.discovery

db = redis.StrictRedis(decode_responses=True, port=6380)

import argparse
parser = argparse.ArgumentParser(description="""Get some tokens.""")

parser.add_argument('-a','--account',
                    type=str,
                    required=False,
                    default="")

args = parser.parse_args()



# This OAuth 2.0 access scope allows for full read/write access to the
# authenticated user's account and requires requests to use an SSL connection.
SCOPES = ["https://www.googleapis.com/auth/calendar",
          "https://www.googleapis.com/auth/calendar.events",
          "https://www.googleapis.com/auth/calendar.events.readonly",
          "https://www.googleapis.com/auth/calendar.readonly",
          "https://www.googleapis.com/auth/calendar.settings.readonly"]
API_SERVICE_NAME = 'calendar'
API_VERSION = 'v3'

# SCOPES = ["https://mail.google.com/"]
# API_SERVICE_NAME = "gmail"
# API_VERSION = "v1"

app = flask.Flask(__name__)
# Note: A secret key is included in the sample so that it works.
# If you use this code in your application, replace this with a truly secret
# key. See http://flask.pocoo.org/docs/0.12/quickstart/#sessions.
app.secret_key = 'REPLACE ME - this value is here as a placeholder.'


@app.route('/')
def index():
    return print_index_table()


@app.route('/record')
def record_api_tokens():
    if 'credentials' not in flask.session:
        return flask.redirect('authorize')

    # Load credentials from the session.
    credentials = google.oauth2.credentials.Credentials(
        **flask.session['credentials'])

    calendar = googleapiclient.discovery.build(
        API_SERVICE_NAME, API_VERSION, credentials=credentials)

    message = f"""Token: {credentials.token}
Refresh token: {credentials.refresh_token}"""

    print(message)
    flask.session['credentials'] = credentials_to_dict(credentials)
    db.set(f"{args.account}_access_token", credentials.token)
    db.set(f"{args.account}_refresh_token", credentials.refresh_token)

    return message.replace("\n", "<br/>")


def get_flow():
    return google_auth_oauthlib.flow.Flow.from_client_config(
        {
            "installed": {
                "client_id": db.get("client_id"),
                "auth_uri": "https://accounts.google.com/o/oauth2/auth",
                "token_uri": "https://oauth2.googleapis.com/token",
                "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
                "client_secret": db.get("client_secret"),
                "redirect_uris": [
                    "urn:ietf:wg:oauth:2.0:oob",
                    "http://localhost"
                ]
            }
        },
        scopes=SCOPES)


@app.route('/authorize')
def authorize():
    flow = get_flow()
    flow.redirect_uri = flask.url_for('oauth2callback', _external=True)

    authorization_url, state = flow.authorization_url(
        access_type='offline',
        include_granted_scopes='true')

    # Store the state so the callback can verify the auth server response.
    flask.session['state'] = state

    return flask.redirect(authorization_url)


@app.route('/oauth2callback')
def oauth2callback():
    state = flask.session['state']

    flow = get_flow()
    flow.redirect_uri = flask.url_for('oauth2callback', _external=True)

    authorization_response = flask.request.url
    flow.fetch_token(authorization_response=authorization_response)
    credentials = flow.credentials
    flask.session['credentials'] = credentials_to_dict(credentials)
    return flask.redirect(flask.url_for('record_api_tokens'))


@app.route('/revoke')
def revoke():
    if 'credentials' not in flask.session:
        return ('You need to <a href="/authorize">authorize</a> before ' +
                'testing the code to revoke credentials.')

    credentials = google.oauth2.credentials.Credentials(
        **flask.session['credentials'])

    revoke = requests.post('https://accounts.google.com/o/oauth2/revoke',
                           params={'token': credentials.token},
                           headers = {'content-type': 'application/x-www-form-urlencoded'})

    status_code = getattr(revoke, 'status_code')
    if status_code == 200:
        return('Credentials successfully revoked.' + print_index_table())
    else:
        return('An error occurred.' + print_index_table())


@app.route('/clear')
def clear_credentials():
    if 'credentials' in flask.session:
        del flask.session['credentials']
    return ('Credentials have been cleared.<br><br>' +
            print_index_table())


def credentials_to_dict(credentials):
    return {'token': credentials.token,
            'refresh_token': credentials.refresh_token,
            'token_uri': credentials.token_uri,
            'client_id': credentials.client_id,
            'client_secret': credentials.client_secret,
            'scopes': credentials.scopes}

def print_index_table():
    return (
        f"Account: {args.account}<br/>" +
        '<table>' +
        '<tr><td><a href="/authorize">Record a token</a></td>' +
        '<td>Go directly to the authorization flow to fetch and save the tokens.' +
        '    You still might not be prompted to reauthorize ' +
        '    the application.</td></tr>' +
        '<tr><td><a href="/revoke">Revoke current credentials</a></td>' +
        '<td>Revoke the access token associated with the current user ' +
        '    session. After revoking credentials, if you go to the test ' +
        '    page, you should see an <code>invalid_grant</code> error.' +
        '</td></tr>' +
        '<tr><td><a href="/clear">Clear Flask session credentials</a></td>' +
        '<td>Clear the access token currently stored in the user session. ' +
        '    After clearing the token, if you <a href="/test">test the ' +
        '    API request</a> again, you should go back to the auth flow.' +
        '</td></tr></table>')


if __name__ == '__main__':
    os.environ['OAUTHLIB_INSECURE_TRANSPORT'] = '1'
    app.run('localhost', 9675, debug=True)
