#!/usr/bin/env python

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4

import logging, sys, os, random, re, datetime, time, glob, tempfile, subprocess

def intervalarg(string):
    m = re.search(r'^(\d*\.?\d*)\s*([smhd])$', string, re.I)
    if m == None:
        msg = '%r is not a date interval' % string
        raise parse.ArgumentTypeError(msg)
    n = m.group(1)
    n = float(n)

    suffix = m.group(2).lower()
    if suffix == 's':
        rv = datetime.timedelta(seconds=n)
    elif suffix == 'm':
        rv = datetime.timedelta(minutes=n)
    elif suffix == 'h':
        rv = datetime.timedelta(hours=n)
    elif suffix == 'd':
        rv = datetime.timedelta(days=n)
    else:
        msg = 'bad unit %s in date interval %s' % (suffix, string)
        raise argparse.ArgumentTypeError(msg)

    return rv

def submit(last_submissions):
    files = glob.glob('./*.jcl')
    files.extend(glob.glob('./*.JCL'))

    picked = None
    now = datetime.datetime.now()

    i = 0

    while picked is None and i < 5:
        job_lines = []
        picked = random.choice(files)
        info = {'min': '5s'}
        with open(picked,'rU') as f:
            for line in f:
                logging.debug(" in: %s", ":".join("{:02x}".format(ord(c)) for c in line))
                jobcard = re.match(r'^//(\S+)(\s+JOB\s+.*$)', line, re.S)
                if jobcard:
                    jobname = jobcard.group(1)
                    rest_of_jobcard = jobcard.group(2)
                    new_jobname = ''
                    for c in jobname:
                        if c == 'n':
                            new_jobname += random.choice(list('0123456789'))
                        elif c == 'a':
                            new_jobname += random.choice(list('ABCDEFGHIJKLMNOPQRSTUVWXYZ'))
                        else:
                            new_jobname += c
                    if new_jobname != jobname:
                        logging.debug ('jobname changed from %s to %s', jobname, new_jobname)
                    line = '//' + new_jobname + rest_of_jobcard
                    job_lines.append(line)
                    continue

                min_card = re.match(r'^//\*MIN:\s+(\S+).*$', line)
                if min_card:
                    min_interval = intervalarg(min_card.group(1))
                    info['min'] = min_card.group(1)
                    job_lines.append(line)
                    continue

                job_lines.append(line)

        if info.get('min') is not None:
            min_interval = intervalarg(info.get('min'))
            last_submission = last_submissions.get(picked)
            if last_submission is not None and now < (last_submission + min_interval):
                logging.debug ('Cannot submit %s: too soon', picked)
                picked = None

        if picked is None:
            i = i + 1

    if picked is not None:
        for l in job_lines:
            logging.debug("out: %s", ":".join("{:02x}".format(ord(c)) for c in l))
        last_submissions[picked] = now
        (fd, fn) = tempfile.mkstemp(suffix='.tjcl', text=True)
        with os.fdopen(fd, 'w') as f:
            f.writelines(job_lines)
        logging.info('submitting %s from file %s', picked, fn)
        subprocess.call('nc -w1 localhost 3505 < ' + fn, shell=True)
        os.remove(fn)

def main():
    import argparse

    parser = argparse.ArgumentParser(description='Keep throwing random jobs at MVS')
    parser.add_argument('--verbose', action='count', help='crank up logging')

    args = parser.parse_args()

    logging.basicConfig (level=logging.INFO, format='%(asctime)s %(levelname)s %(name)s %(message)s')
    if args.verbose > 0:
        logging.getLogger().setLevel(logging.DEBUG)

    last_submissions = {}

    while True:
        submit(last_submissions)
        time.sleep(1)

if __name__ == '__main__':
    main()
