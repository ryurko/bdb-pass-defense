
import pandas as pd

weekly_data = []

for w in range(1,18):
    result = pyreadr.read_r('../../data/weekly/week_'+str(w)+'_model_data.rds')[None]
    result = result[result['is_start_bc'] == 1].reset_index(drop=True)
    weekly_data.append(result)
    
play_context = pd.read_csv('../../data/play_level_features.csv')
play_features = list(play_context.columns)[2:]
metafeatures = pyreadr.read_r('../../data/at_catch_yac_model_data.rds')[None][['game_play_id','bc_dist_to_qb']]
new = metafeatures["game_play_id"].str.split("_", n = 1, expand = True)
metafeatures['playId'] = pd.to_numeric(new[1])
metafeatures['gameId'] = pd.to_numeric(new[0])
metafeatures.drop(columns = 'game_play_id')
context = pd.merge(play_context,metafeatures,on= ['playId','gameId'])
context = context.drop(columns = 'game_play_id')
play_features = list(context.columns)[2:]

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

def unison_shuffled_copies(a, b, c):
        p = np.random.permutation(len(a))
        return a[p], b[p], c[p]

def prepareXY(weekly_data,context,week, defender):
    train_X = pd.DataFrame(columns = ['gameId','playId','bc_x','bc_y','bc_s','bc_dir','defense_1_x','defense_1_y','defense_1_s','defense_1_dir',
                                      'defense_2_x','defense_2_y','defense_2_s','defense_2_dir','defense_3_x','defense_3_y','defense_3_s','defense_3_dir',
                                      'defense_4_x','defense_4_y','defense_4_s','defense_4_dir','offense_1_x','offense_1_y','offense_1_s','offense_1_dir',
                                      'offense_2_x','offense_2_y','offense_2_s','offense_2_dir','offense_3_x','offense_3_y','offense_3_s','offense_3_dir',
                                      'offense_4_x','offense_4_y','offense_4_s','offense_4_dir']) 
    for i in range(17):
        if i == week-1:
            continue
        tmp = weekly_data[i][['gameId','playId','bc_x','bc_y','bc_s','bc_dir','defense_1_x','defense_1_y','defense_1_s','defense_1_dir',
                                      'defense_2_x','defense_2_y','defense_2_s','defense_2_dir','defense_3_x','defense_3_y','defense_3_s','defense_3_dir',
                                      'defense_4_x','defense_4_y','defense_4_s','defense_4_dir','offense_1_x','offense_1_y','offense_1_s','offense_1_dir',
                                      'offense_2_x','offense_2_y','offense_2_s','offense_2_dir','offense_3_x','offense_3_y','offense_3_s','offense_3_dir',
                                      'offense_4_x','offense_4_y','offense_4_s','offense_4_dir']]
        train_X = pd.concat([train_X,tmp])
    train_X = pd.merge(train_X,context,on= ['playId','gameId'])
    train_X = train_X[['bc_x','bc_y','bc_s','bc_dir','defense_1_x','defense_1_y','defense_1_s','defense_1_dir',
                        'defense_2_x','defense_2_y','defense_2_s','defense_2_dir','defense_3_x','defense_3_y','defense_3_s','defense_3_dir',
                        'defense_4_x','defense_4_y','defense_4_s','defense_4_dir','offense_1_x','offense_1_y','offense_1_s','offense_1_dir',
                        'offense_2_x','offense_2_y','offense_2_s','offense_2_dir','offense_3_x','offense_3_y','offense_3_s','offense_3_dir',
                        'offense_4_x','offense_4_y','offense_4_s','offense_4_dir'] + list(context.columns)[2:]]
    # normalize x, y, speed and direction
    v1 = [120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360]
    v2 = [120,4,4,5,11,11,3,6,3,7,7,7,15*60,3,30*60,60*60,1,1,1,1,1,1,1,1,1,1,1,1,1,1,60]
    norm_vect = v1+v2
    train_X = train_X / norm_vect
    train_X = train_X.dropna()
    y = np.array(train_X[['defense_'+str(defender)+'_x','defense_'+str(defender)+'_y']])
    train_X = train_X.drop(['defense_'+str(defender)+'_x','defense_'+str(defender)+'_y'],axis=1)
    X = np.array(train_X)

    return X,y

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
        traj_in = Input(shape=(params.seq_len,65),  name='main_input')
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
        return model


# start training

decl = lambda w, d, b, sl, lu, ll, dr: "week="+str(w)+"-defender"+str(d)+"-batch="+str(b)+"-seq_len="+str(sl)+"-LSTMunits="+str(lu)+"-LSTMlayers="+str(ll)+"-dropout="+str(dr)

role_train = 1
batch_sz = [128]
lu = [32]
dr = [0.2]
ll = [3]
sls = [1]

week_mod = {1: [], 2: [], 3: [], 4: []}

for bs in batch_sz:
        for luu in lu:
                for drr in dr:
                        for lll in ll:
                                for sl in sls:
                                    for week_out in range(1,18):
                                        for defender in [1,2,3,4]:
                                            print("+++++++++++++++++++++++++++++++++")
                                            print("Training for: ",role_train, bs, luu, lll, drr, week_out, defender)
                                            fldr = decl(week_out,defender, bs, sl, luu, lll, drr)
                                            X, y = prepareXY(weekly_data,context,week_out,defender)
                                            week_mod[defender].append(train(Params(batch_sz = bs, seq_len = sl,
                                                                         lstm_args = [(luu,)]*lll, drout = drr, 
                                                                         role = role_train, folder = fldr),
                                                                         X.reshape((X.shape[0],1,65)),y))
                                            print(week_mod)



## find the ghosts for each play in a leave-one-week-out manner

ghosts_df = pd.DataFrame(columns=['game_play_id','defense_1_x_ghost', 'defense_1_y_ghost','defense_2_x_ghost', 
                                  'defense_2_y_ghost', 'defense_3_x_ghost', 'defense_3_y_ghost',
                                  'defense_4_x_ghost', 'defense_4_y_ghost'])

for week in range(1,18):
    print("Week: ",week)
    test = weekly_data[week-1]
    test = pd.merge(test,context,on= ['playId','gameId'])
    for p in range(0,len(test)):
        ghosts = []
        inp = test.iloc[p][['bc_x','bc_y','bc_s','bc_dir','defense_1_x','defense_1_y','defense_1_s','defense_1_dir',
                        'defense_2_x','defense_2_y','defense_2_s','defense_2_dir','defense_3_x','defense_3_y','defense_3_s','defense_3_dir',
                        'defense_4_x','defense_4_y','defense_4_s','defense_4_dir','offense_1_x','offense_1_y','offense_1_s','offense_1_dir',
                        'offense_2_x','offense_2_y','offense_2_s','offense_2_dir','offense_3_x','offense_3_y','offense_3_s','offense_3_dir',
                        'offense_4_x','offense_4_y','offense_4_s','offense_4_dir']+list(context.columns)[2:]]
        v1 = [120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360,120.0,53.3,10,360]
        v2 = [120,4,4,5,11,11,3,6,3,7,7,7,15*60,3,30*60,60*60,1,1,1,1,1,1,1,1,1,1,1,1,1,1,60]
        norm_vect = v1+v2
        inp = inp/ norm_vect
        for defender in [1,2,3,4]:
            inpt = inp.drop(['defense_'+str(defender)+'_x','defense_'+str(defender)+'_y'])
            pred = np.multiply(week_mod[defender][week-1].predict(np.array([inpt]).reshape(1,1,65)),np.array([120,53.3]))
            ghosts.append(pred[0][0])
            ghosts.append(pred[0][1])
        ghosts = [test.iloc[p]['game_play_id']]+ghosts
        ghosts_df.loc[len(ghosts_df)] = ghosts
        
# save the ghosts to a file

ghosts_df.to_csv("ghosts_at_catch.csv",index=False)
