from keras.models import load_model
import pandas as pd
import numpy as np
import sys

models = []

for w in range(1,18):
    m = [f for f in sorted(os.listdir('traj_models_def_seq2seq/week='+str(w)+'-batch=128-seq_len=1-LSTMunits=32-LSTMlayers=3-dropout=0.2/')) if 'hdf5' in f][-1]
    models.append(keras.models.load_model('traj_models_def_seq2seq/week='+str(w)+'-batch=128-seq_len=1-LSTMunits=32-LSTMlayers=3-dropout=0.2/'+m))



data = pd.read_csv("completed_pass_plays.csv")
data = data[(data['dir'].notnull()) | (data['team'] == "football")]
data['x'] = data['x']/120.0
data['y'] = data['y']/53.3
data['s'] = data['s']/max(data['s'])
data['a'] = data['a']/max(data['a'])
data['o'] = data['o']/max(data['o'])
data['dir'] = data['dir']/max(data['dir'])
#data['ghost_X'] = list(np.zeros(len(data)))
#data['ghost_Y'] = list(np.zeros(len(data)))
games = pd.read_csv("games.csv")
games_d = dict(zip(list(games['gameId']), list(games['week']))) 
week = [games_d[i] for i in list(data['gameId'])]
data['week'] = week
plays=data.groupby(by=["key"])
pii = 0

ghosts_df = pd.DataFrame(columns = list(data.columns))

ghosts_X = []
ghosts_Y = []

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
            x_f += list(np.array(ball[ball.team=="football"][['x','y']]).reshape((2,)))
            X_train[pii,:] = np.array(x_f) 
            pred= np.multiply(models[int(ball['week'])-1].predict(np.array([X_train[pii]]).reshape(1,1,6)),np.array([120,53.3]))
            ghosts_X.append(pred[0][0])
            ghosts_Y.append(pred[0][1])
            ghosts_df = ghosts_df.append(tmp_1[tmp_1.nflId == defender_id],ignore_index=True)
            pii += 1
    except Exception as e:
        exc_type, exc_obj, exc_tb = sys.exc_info()
        print(exc_type, exc_tb.tb_lineno)

ghosts_df['ghost_X'] = ghosts_X
ghosts_df['ghost_Y'] = ghosts_Y
ghosts_df[['x','y','ghost_X','ghost_Y']]
ghosts_df['x'] = ghosts_df['x']*120.0
ghosts_df['y'] = ghosts_df['y']*53.3
ghosts_df[['x','y','ghost_X','ghost_Y']]
ghosts_df.to_csv('ghosts.csv',index=False)
