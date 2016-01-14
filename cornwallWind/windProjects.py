import urllib
import sqlite3
import json
import time
import ssl

serviceurl = "http://maps.googleapis.com/maps/api/geocode/json?"

# Deal with SSL certificate anomalies Python > 2.7
# scontext = ssl.SSLContext(ssl.PROTOCOL_TLSv1)
scontext = None

conn = sqlite3.connect('windProjects.sqlite')
cur = conn.cursor()
#DROP TABLE IF EXISTS Projects;
cur.executescript('''

CREATE TABLE IF NOT EXISTS Projects (
    project TEXT,
    region TEXT,
    location TEXT,
    turbines INT,
    project_capacity FLOAT,
    turbine_capacity FLOAT,
    developer TEXT,
    current_status_date TEXT,
    status TEXT,
    onoff TEXT,
    geodata TEXT
    )''')

fh = open("turbines.txt","r")
print "Hello"
count = 0
for line in fh:
    #print "Hello",line
    count += 1
nProjects=count/10
print "nProjects: ",nProjects
fh.close()
fh = open("turbines.txt","r")

for i in range (nProjects):
    print i
    project =fh.readline().strip()

    print ''
    cur.execute("SELECT project FROM Projects WHERE project= ?", (buffer(project), ))

    try:
        data = cur.fetchone()[0]
        print "Found in database ",project
        continue
    except:
        pass
    
    region =fh.readline().strip()
    #print region
    location =fh.readline().strip()+(", Cornwall, UK")
    #print location
    turbines =int(fh.readline().strip())
    #print turbines
    project_capacity =float(fh.readline().strip())
    turbine_capacity =float(fh.readline().strip())
    developer =fh.readline().strip()
    current_status_date =fh.readline().strip()
    status =fh.readline().strip()
    onoff =fh.readline().strip()
        
    print 'Resolving', project
    url = serviceurl + urllib.urlencode({"sensor":"false", "address": location})
    print 'Retrieving', url
    uh = urllib.urlopen(url, context=scontext)
    data = uh.read()
    print 'Retrieved',len(data),'characters',data[:20].replace('\n',' ')
    count = count + 1
    try: 
        js = json.loads(str(data))
        # print js  # We print in case unicode causes an error
    except: 
        continue

    if 'status' not in js or (js['status'] != 'OK' and js['status'] != 'ZERO_RESULTS') : 
        print '==== Failure To Retrieve ===='
        print data
        break

    cur.execute('''INSERT INTO Projects (project,region,location,turbines,project_capacity,turbine_capacity,developer,current_status_date,status,onoff,geodata)
              VALUES ( ?,?,?,?,?,?,?,?,?,?,? ) ''',(project,region,location,turbines,project_capacity,turbine_capacity,developer,current_status_date,status,onoff,buffer(data)))
    conn.commit()
 
    time.sleep(1)
fh.close()