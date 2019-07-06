import tweepy
import subprocess

consumer_key = 'YqxmomIx9kFH2VfJzGrVd4M1H'
consumer_secret = 'KfJrH90L5El6aJinNVlRjrmdce9Kv2xwFR27nfZFG6goSYQkom'
access_token = '1139681970646990849-HK1NQf0DIILFf6xnMVLxlOyL4YFEaa'
access_token_secret = '26K1DMlKgvN4Te1VHJFF0DVYXIADc64RTLVQI05GT6SgM'

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)
user = api.me()

subprocess.call("Rscript sports_guy.R", shell=True)

def file_get_contents(filename):
    with open(filename) as f:
        return f.read()

subprocess.call("head -c280 output.txt > output_trimmed.txt", shell=True)

tweet = file_get_contents("output_trimmed.txt")

api.update_status(tweet)

subprocess.call("rm output*", shell=True)


