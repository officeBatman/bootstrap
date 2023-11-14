import os.path

FILENAME = "bootstrap.h"
PATH = os.path.join(os.path.dirname(__file__), FILENAME)
TARGET_DIRECTORIES = ['rewrite', 'examples', 'delete']

print(f'Updating bootstrap headers in {TARGET_DIRECTORIES}...')

print(f'Reading {PATH}...')
with open(PATH, 'r') as f:
    content = f.read()

for directory in TARGET_DIRECTORIES:
    # Ignore directories that don't exist
    dir_path = os.path.join(os.path.dirname(__file__), directory)
    if not os.path.exists(dir_path):
        print(f'Ignoring {dir_path}...')
        continue

    # Write the file
    target_path = os.path.join(dir_path, FILENAME)
    print(f'Writing {target_path}...')
    with open(target_path, 'w') as target:
        target.write(content)

print('Done!')
