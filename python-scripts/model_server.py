from flask import Flask
import sys
import gzip
from flask import request
import pandas as pd
import numpy as np
import cPickle as pickle
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import LabelEncoder
from scipy import sparse
import cStringIO as StringIO

import collections

class LRUCache:
    def __init__(self, capacity):
        self.capacity = capacity
        self.cache = collections.OrderedDict()

    def __getitem__(self, key):
        try:
            value = self.cache.pop(key)
            self.cache[key] = value
            return value
        except KeyError:
            return -1

    def __setitem__(self, key, value):
        try:
            self.cache.pop(key)
        except KeyError:
            if len(self.cache) >= self.capacity:
                self.cache.popitem(last=False)
        self.cache[key] = value

    def __contains__(self, key):
        return key in self.cache
    def __len__(self):
        return len(self.cache)


class ColumnChooser(object):
    def __init__(self, name, fillna=None):
        self.name = name
        self.fillna = fillna
    def fit(self, X, y=None):
        return self
    def transform(self, X, y=None):
        res = X.loc[:, self.name]
        if (self.fillna is not None):
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
            self.enc.fit(enc)
        except:
            self.failed = True
        return self
    def transform(self, X, y=None):
        if self.failed:
            return sparse.csr_matrix([[]] * X.shape[0])
        else:
            return self.enc.transform(X)

models = {}
models_cache = LRUCache(1000) # if we start to have mem problems -- just use LRU cahce

app = Flask(__name__)

@app.route('/prob', methods=['POST'])
def eval_prob():
    text = request.data.strip().split('\n')
    resp = []
    try:
      for i in xrange(0, len(text), 2):
        indata = '\n'.join(text[i:i+2])
        df = pd.read_csv(StringIO.StringIO(indata), sep='\t')
        #print '------ DATA -----'
        #print df.head()
       
        for i in xrange(len(df)):
          line = df.iloc[i]
          frame = line['frame']
          if frame in models:
             try:
                  model_path = models[frame]
                  if model_path in models_cache:
                      model = models_cache[model_path]
                  else:
                      model = pickle.load(gzip.open(model_path, 'r'))
                      models_cache[model_path] = model
                      print 'loaded model %s -- total models: %d' % (model_path, len(models_cache))
                  #if len(model.steps[0][1].transformer_list[3][1].steps[1][1].enc.classes_) == 1:
                  #  df.iloc[i:i+1]['predicate_aspect'] = model.steps[0][1].transformer_list[3][1].steps[1][1].enc.classes_[0]
                  #print model
                  #print ' ---- FRAME ', frame
                  #print ' --- FRAME DATA.... '
                  #print df.iloc[i:i+1]
                  #print ' --- PREDICTION RESULTS' 
                  #print (model.predict_proba(df.iloc[i:i+1]))
                  #print ' --- PROBABILITY RETURNED'
                  #print (model.predict_proba(df.iloc[i:i+1])[0, 1])
                     
                  resp.append('%.6f' % (model.predict_proba(df.iloc[i:i+1])[0, 1]))
             except:
                 resp.append('NaN')
          else:
             # print 'EXCEPTION!!! ', sys.exc_info(), text
             resp.append('NaN')
  
      return '\n'.join(resp) 
    except:
       print 'EXCEPTION!!! ', sys.exc_info(), text
       return ''
  

if __name__ == '__main__':
  model_path = sys.argv[1]
  with open(model_path, 'r') as fw:
    for line in fw.xreadlines():
      (path, frame) = map(str.strip, line.split(': ', 1))
      models[frame] = path

  #while True:
  #    try:
  app.run(host='0.0.0.0', port=int(sys.argv[2]), debug=False) #, threaded=True, debug=True)
  #    except:
  #        print 'Ignoring error', sys.exc_info()

      
 
