import pandas as pd
import numpy as np
import sys

data = pd.read_csv("completed_pass_plays.csv")
data = data[(data['dir'].notnull()) | (data['team'] == "football")]
data['x'] = data['x']/120.0
data['y'] = data['y']/53.3
data['s'] = data['s']/max(data['s'])
data['dir'] = data['dir']/max(data['dir'])

games = pd.read_csv("games.csv")
games_d = dict(zip(list(games['gameId']), list(games['week']))) 
week = [games_d[i] for i in list(data['gameId'])]
data['week'] = week

plays=data.groupby(by=["key"])

defense = ["FS","SS","S","CB"]
offense = ['WR','QB','RB','TE']

X_train = np.zeros((8*len(set(data['key'])),6))
y_train = np.zeros((8*len(set(data['key'])),4))
week_train = np.zeros(8*len(set(data['key'])))

pii = 0

for p in set(data['key']):
    try:
        
        tmp = plays.get_group(p)
        tmp_0 = tmp[tmp['event'] == "pass_forward"]
        tmp_1 = tmp[tmp['event'] == "pass_outcome_caught"]
        
        pos0 = tmp_0[tmp_0.position.isin(defense)].reset_index()
        ball = tmp_1[tmp_1.team == "football"]
        for d in range(len(pos0)):
            x_f = []
            x_f = list(np.array(pos0.iloc[d][['x','y','s','dir']]).reshape((4,)))
            defender_id = pos0.iloc[d]['nflId'] 
            y = np.array(tmp_1[tmp_1.nflId == defender_id][['x','y','s','dir']]).reshape((4,))
            x_f += list(np.array(ball[ball.team=="football"][['x','y']]).reshape((2,)))
            X_train[pii,:] = np.array(x_f) 
            y_train[pii,:] = y
            week_train[pii] = float(ball['week'])
            pii += 1
            
    except Exception as e:
        exc_type, exc_obj, exc_tb = sys.exc_info()
        print(exc_type, exc_tb.tb_lineno)
        
import keras
import sys
from sklearn.model_selection import train_test_split
from keras.models import Model
from keras.callbacks import EarlyStopping
from keras.callbacks import ModelCheckpoint
from keras.layers import Input, Dropout, LSTM, Dense
import numpy as np
import tensorflow as tf
import glob
import os
import pickle

def unison_shuffled_copies(a, b):
        assert len(a) == len(b)
        p = np.random.permutation(len(a))
        return a[p], b[p]

class Params:

        def __init__(self, batch_sz, seq_len, lstm_args, drout, role, folder):
                self.batch_sz = batch_sz
                self.seq_len = seq_len
                self.lstm_args = lstm_args
                self.drout = drout
                self.role = role
                self.folder = folder
                if not os.path.exists(folder):
                        os.mkdir(folder)
                with open(folder + '/params.pickle', 'wb') as p:
                        pickle.dump(self, p)
                        
def build_model(params):
        traj_in = Input(shape=(params.seq_len,4),  name='main_input')
        play_rep = traj_in
        for i, args in enumerate(params.lstm_args):
                units = args[0]
                rseq = True if i != len(params.lstm_args) - 1 else False
                play_rep = keras.layers.LSTM(units,return_sequences = rseq)(play_rep)

        dense = keras.layers.Dense(units = 32)(play_rep)
        drop = keras.layers.Dropout(rate = params.drout)(dense)
        nout = 2
        ghosts = keras.layers.Dense(nout)(drop)

        model = keras.Model(inputs = [traj_in], outputs = [ghosts])
        return model

def train(params,x1_train,y_train):
        model_check_point = ModelCheckpoint(params.folder+"/model_tr.{epoch:02d}-{val_loss:.5f}.hdf5",monitor = 'val_loss',mode = 'min',save_best_only = True)
        early_stopping = EarlyStopping(monitor = 'val_loss',min_delta = 0.00001,patience = 5)
        model = build_model(params)
        model.summary()
        model.compile(optimizer=keras.optimizers.Adadelta(),loss='mean_squared_error')
        model.fit([x1_train],[y_train],epochs=100, callbacks = [model_check_point, early_stopping],  batch_size = params.batch_sz, validation_split = 0.2)

X_train = X_train[:pii]
y_train = y_train[:pii]
X_traj_shuffled, y_traj_shuffled = unison_shuffled_copies(X_train, y_train)
X_traj_shuffled = X_traj_shuffled.reshape((X_traj_shuffled.shape[0],1,X_traj_shuffled.shape[1]))

# just for some quick out-of-sample testing for now
X_test = X_traj_shuffled[0:10]
y_test = y_traj_shuffled[0:10]

X_traj_shuffled = X_traj_shuffled[10:]
y_traj_shuffled = y_traj_shuffled[10:]

# start training

decl = lambda r, b, sl, lu, ll, dr: "traj_models_def_seq2seq/role="+str(r)+"-batch="+str(b)+"-seq_len="+str(sl)+"-LSTMunits="+str(lu)+"-LSTMlayers="+str(ll)+"-dropout="+str(dr)

role_train = 1
batch_sz = [512]
lu = [32]
dr = [0.2]
ll = [1]
sls = [1]


for bs in batch_sz:
        for luu in lu:
                for drr in dr:
                        for lll in ll:
                                for sl in sls:
                                        print("+++++++++++++++++++++++++++++++++")
                                        print("Training for: ",role_train, bs, luu, lll, drr)
                                        fldr = decl(role_train,bs, sl, luu, lll, drr)
                                        train(Params(batch_sz = bs, seq_len = sl,lstm_args = [(luu,)]*lll, drout = drr, role = role_train, folder = fldr),X_traj_shuffled,y_traj_shuffled)

