import pyreadr
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

weekly_data = []

ghosts = pd.read_csv("ghosts_at_catch.csv")

defender_1 = []
defender_2 = []
defender_3 = []
defender_4 = []

for w in range(1,18):
    result = pyreadr.read_r('../input/create-weekly-model-datasets/week_'+str(w)+'_model_data.rds')[None]
    result = result[result['is_start_bc'] == 1].reset_index(drop=True)
    data = pd.merge(result,ghosts, on =['game_play_id']).reset_index(drop=True)
    defender_1 += [np.linalg.norm(np.array(data[['defense_1_x','defense_1_y']].iloc[i]) - np.array(data[['defense_1_x_ghost','defense_1_y_ghost']].iloc[i])) for i in range(len(data)) ]
    defender_2 += [np.linalg.norm(np.array(data[['defense_2_x','defense_2_y']].iloc[i]) - np.array(data[['defense_2_x_ghost','defense_2_y_ghost']].iloc[i])) for i in range(len(data)) ]
    defender_3 += [np.linalg.norm(np.array(data[['defense_3_x','defense_3_y']].iloc[i]) - np.array(data[['defense_3_x_ghost','defense_3_y_ghost']].iloc[i])) for i in range(len(data)) ]
    defender_4 += [np.linalg.norm(np.array(data[['defense_4_x','defense_4_y']].iloc[i]) - np.array(data[['defense_4_x_ghost','defense_4_y_ghost']].iloc[i])) for i in range(len(data)) ]
    
    
df1 = pd.DataFrame({"Distance (yards)":defender_1,"Defender":['Closest' for _ in range(len(defender_1))]})
df2 = pd.DataFrame({"Distance (yards)":defender_2,"Defender":['Second closest' for _ in range(len(defender_2))]})
df3 = pd.DataFrame({"Distance (yards)":defender_3,"Defender":['Third closest' for _ in range(len(defender_3))]})
df4 = pd.DataFrame({"Distance (yards)":defender_4,"Defender":['Fourth closest' for _ in range(len(defender_4))]})
df = pd.concat([df1,df2,df3,df4])


fig = plt.figure()


fig.suptitle('Distribution of distance between ghosts and\n actual defenders from all the plays', fontsize=13)

ax = fig
ax = sns.violinplot(x="Defender", y="Distance (yards)", data=df)

fig.savefig("violin.png")    

## estimate per week

d1 = []
d2 = []
d3 = []
d4 = []

for w in range(1,18):
    defender_1 = []
    defender_2 = []
    defender_3 = []
    defender_4 = []
    result = pyreadr.read_r('../input/create-weekly-model-datasets/week_'+str(w)+'_model_data.rds')[None]
    result = result[result['is_start_bc'] == 1].reset_index(drop=True)
    data = pd.merge(result,ghosts, on =['game_play_id']).reset_index(drop=True)
    defender_1 += [np.linalg.norm(np.array(data[['defense_1_x','defense_1_y']].iloc[i]) - np.array(data[['defense_1_x_ghost','defense_1_y_ghost']].iloc[i])) for i in range(len(data)) ]
    defender_2 += [np.linalg.norm(np.array(data[['defense_2_x','defense_2_y']].iloc[i]) - np.array(data[['defense_2_x_ghost','defense_2_y_ghost']].iloc[i])) for i in range(len(data)) ]
    defender_3 += [np.linalg.norm(np.array(data[['defense_3_x','defense_3_y']].iloc[i]) - np.array(data[['defense_3_x_ghost','defense_3_y_ghost']].iloc[i])) for i in range(len(data)) ]
    defender_4 += [np.linalg.norm(np.array(data[['defense_4_x','defense_4_y']].iloc[i]) - np.array(data[['defense_4_x_ghost','defense_4_y_ghost']].iloc[i])) for i in range(len(data)) ]
    d1.append(np.nanmean(defender_1))
    d2.append(np.nanmean(defender_2))
    d3.append(np.nanmean(defender_3))
    d4.append(np.nanmean(defender_4))
    
df1 = pd.DataFrame({"Distance (yards)":d1,"Defender":['Closest' for _ in range(len(d1))]})
df2 = pd.DataFrame({"Distance (yards)":d2,"Defender":['Second closest' for _ in range(len(d2))]})
df3 = pd.DataFrame({"Distance (yards)":d3,"Defender":['Third closest' for _ in range(len(d3))]})
df4 = pd.DataFrame({"Distance (yards)":d4,"Defender":['Fourth closest' for _ in range(len(d4))]})
df = pd.concat([df1,df2,df3,df4])

fig = plt.figure()

axs = fig
plt.ylim(0,15)

fig.suptitle('Average distance between ghosts and defenders. \n Each point corresponds to a week', fontsize=13)

axs = sns.swarmplot(x="Defender", y="Distance (yards)", data=df)
fig.savefig("swarmplot.png")
