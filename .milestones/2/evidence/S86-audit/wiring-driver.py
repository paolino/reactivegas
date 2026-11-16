import sys
sys.argv.append('import')
from probe import *
for num,mode in [(34,'removed'),(35,'bypass'),(36,'candidate')]:
 result=run(f'P{num}-wiring-{mode}','committed CI step removal/bypass detector, actual command execution',['/nix/store/3lll9y925zz9393sa59h653xik66srjb-python3-3.13.9/bin/python3',R/'wiring.py',mode],expected='accept' if mode=='candidate' else 'reject')
 print(result.stdout.decode().splitlines()[-4:])
 assert b'DETECTOR '+(b'PASS' if mode=='candidate' else b'FAIL') in result.stdout
