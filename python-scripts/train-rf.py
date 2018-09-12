#!/usr/bin/env python
import cPickle as pickle
import glob
import gzip
import os.path
import time

from scipy import sparse
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import LabelEncoder
import numpy as np
import pandas as pd


class ColumnChooser(object):
    def __init__(self, name, fillna=None):
        self.name = name
        self.fillna = fillna
    def fit(self, X, y=None):
        return self
    def transform(self, X, y=None):
        res = X.loc[:, self.name]
        if self.fillna is not None:
            res = res.fillna(self.fillna)
        return res.values

class MyLabelEncoder(object):
    def __init__(self, allValues=None):
        self.enc = LabelEncoder()
        self.allValues = allValues
    def fit(self, X, y=None):
        if self.allValues is not None:
            self.enc.fit(self.allValues)
        else:
            self.enc.fit(X)
        return self
    def transform(self, X, y=None):
        return self.enc.transform(X)

class Reshaper(object):
    def fit(self, X, y=None):
        return self
    def transform(self, X, y=None):
        return np.reshape(X, (len(X), 1))

class MyTfidfVectorizer(object):
    def __init__(self):
        self.enc = TfidfVectorizer()
        self.failed = False
    def fit(self, X, y=None):
        try:
            self.enc.fit(X)
        except Exception as e:
            self.exception = e
            self.failed = True
        return self
    def transform(self, X, y=None):
        if self.failed:
            return sparse.csr_matrix([[]] * X.shape[0])
        else:
            return self.enc.transform(X)

def train(in_path, out_path):
    data = []
    for x in glob.iglob(in_path + '/*/*.csv.gz'):
        lines = gzip.open(x, 'r').readlines()
        total = len(lines)
        target = len(filter(lambda x: x.strip()[-1] == '1', lines))
        data.append(dict(filename=x, total=total, target=target))
    data = pd.DataFrame(data)

    print 'Frames', len(data)
    print 'Trainable', ((data.target >= 100) & ((data.total - data.target) >= 100)).sum()

    def to_frame_name(f):
        return f[:f.rindex('-')]
    data['frame'] = data.filename.apply(to_frame_name)

    fnames = data.groupby('frame').filename.apply(list).to_dict()

    all_models = []

    bound = 50

    nestimators = 25

    d = data.groupby('frame').agg(dict(target='sum', total='sum')).reset_index()
    trainable = d[((d.target >= bound) & ((d.total - d.target) >= bound))]

    for i, frame in enumerate(trainable.loc[:, 'frame']):
        tt = []
        for path in fnames[frame]:
            t = pd.read_csv((gzip.open(path, 'r')), sep='\t', index_col=False)
            tt.append(t)
        t = pd.concat(tt)
        model_out = os.path.join(out_path, frame.lstrip(in_path)) + '.rf.pkl.gz'
        dirnam = os.path.dirname(model_out)
        if not os.path.exists(dirnam):
            os.makedirs(dirnam)
        domains = [make_pipeline(ColumnChooser(c, fillna=''), MyTfidfVectorizer()) for c in t.columns if c.startswith('domains_')]
        aspect = make_pipeline(ColumnChooser('predicate_aspect'), MyLabelEncoder(["perf", "imperf"]), Reshaper())
        other = [make_pipeline(ColumnChooser(c, fillna=-9999), Reshaper()) for c in t.columns if c[:3] in ('cs_', 'log', 'cla', 'spl', 'has', 'is_')]
        preprocessing = make_union(*(domains + [aspect] + other))
        training = make_pipeline(preprocessing, RandomForestClassifier(n_jobs=40, n_estimators=nestimators, oob_score=True))
        training.fit(t, t.target)
        pickle.dump(training, gzip.open(model_out, 'w'))
        all_models.append((t.iloc[0]['frame'], model_out))
        if (i % 100) == 0:
            print '[', time.time(), ']', (i+1), '/', len(trainable), model_out, '-->', training.steps[1][1].oob_score_

    with open(os.path.join(out_path, 'models.txt'), 'w') as moout:
        for frame, model_out in all_models:
            moout.write(model_out + ': ' + frame + '\n')



if __name__ == '__main__':
    import sys
    if len(sys.argv) < 3:
        print 'Usage: %s csv-directory-in model-directory-out' % sys.argv[0]
        sys.exit(1)
    train(sys.argv[1], sys.argv[2])
