library(DiagrammeR)

#rankdir = LR, label = '\n\n',labelloc = t

#Overall workflow ----
grViz("digraph{
graph [layout = dot]
node [shape = rectangle, style = filled, rankdir = LR, fillcolor = white]

datatrain[label='training data Saskia', shape = folder, fillcolor = beige]
datatestsas[label='test data Saskia', shape = folder, fillcolor = beige]
datatestsar[label='test data Sarah', shape = folder, fillcolor = beige]
classify[label='classify manually \\n into actual walking patterns']

attribute[label='workflow attribute \\n based classification', fillcolor = aliceblue]
derattributes[label='derive attributes']
threshattributes[label='set thresholds based on  \\n summaries of trajectories \\n from training data']
classifyattributes[label='classify based on thresholds']
validate[label='validate with validation workflow']

camawork[label='workflow CAMA \\n based classification', fillcolor = aliceblue]
tlm[label='choose swissTLM3D layers, \\n which represent certain attributes']
buf[label='create buffers']
intersect[label='check for presence in buffers \\n position in buffers']
camaclass[label='classify based on \\n  presence in buffers']

cartwork [label ='workflow CART \\n based classification', fillcolor = aliceblue]
join[label='join data from \\n previous workflows \\n from the training data']
cart [label='create CART']

{datatrain datatestsas datatestsar} ->classify
classify -> {attribute camawork}
attribute -> derattributes
derattributes -> threshattributes
threshattributes -> classifyattributes
classifyattributes -> validate

camawork -> tlm
tlm -> buf
buf -> intersect
intersect -> camaclass
camaclass -> validate

{derattributes intersect} -> cartwork
cartwork -> join
join -> cart
cart -> validate
}")

# Validation workflow ----
grViz("digraph{
graph [layout = dot]
node [shape = rectangle, style = filled, rankdir = LR, fillcolor = white]

valwork[label='validation workflow', fillcolor = aliceblue]
valtrain[label='apply validation steps \\n on results from \\n training data']

applyattr[label='use thresholds \\n from attribute based classification \\n on test data']
applycama[label='use classification rules \\n from CAMA based classification \\n on test data']
applycart[label='apply CART tree to test data']
conf[label='create confusion matix']
acc[label='derive accuracy of classification']

valwork -> {valtrain applyattr applycama applycart}
{valtrain applyattr applycama applycart} -> {conf acc}
}")

#Attribute based ----
grViz("digraph{
graph [layout = dot]
node [shape = rectangle, style = filled, fillcolor = white]

travel[label='mean speed 1.7m/s - 4m/s and mean step 1.7m - 4m and mean acceleration < 0.003m^2']
travelyes[label='Yes']
traveltra[label='Is travel']
travelno[label='No']
recr[label='mean speed 1.1m/s - 1.7m/s and mean step 1.2m - 1.7m and mean acceleration <0.003m^2']
recryes[label='Yes']
recrtra[label='Is recreation']
recrno[label='No']
shop[label='mean speed > 5m/s or < 1.1m/s and mean step > 5m or < 1.2 and mean acceleration >0.001m^2']
shopyes[label='Yes']
shopyesshop[label='Is shopping']
shopno[label='No ']
shoptra[label='Is NA']

travel->{travelyes travelno}
travelyes->traveltra
travelno ->recr

recr -> {recryes recrno}
recryes -> recrtra
recrno -> shop

shop ->{shopyes shopno}
shopyes ->shopyesshop
shopno ->shoptra
}")

#CAMA based
grViz("digraph{
graph [layout = dot]
node [shape = rectangle, style = filled, fillcolor = white]

build[label='Is it in a building?']
buildyes[label='Yes']
buildshop[label='Is shopping']
buildno[label='No']
pubtrans[label='Is it in a 50m radius of a public transport station?']
pubtranspyes[label='Yes']
pubtransptra[label='Is travel']
pubtranspno[label='No']
recr[label=' Is it in a range of 10m of a recreational area?']
recryes[label='Yes']
recryesrec[label='Is recreation']
recrno[label='No ']
recrtra[label='Is travel']

build->{buildyes buildno}
buildyes->buildshop
buildno ->pubtrans

pubtrans -> {pubtranspyes pubtranspno}
pubtranspyes -> pubtransptra
pubtranspno -> recr

recr ->{recryes recrno}
recryes ->recryesrec
recrno ->recrtra
}")
