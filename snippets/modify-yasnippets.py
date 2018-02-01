import os

yasnippet_dir = '/Users/ztlevi/.spacemacs.d/snippets'
modes_dir = os.listdir(yasnippet_dir)
for dir in modes_dir:
    snippets_list_path = os.path.join(yasnippet_dir, dir)
    snippets_list = []
    if os.path.isdir(snippets_list_path):
        snippets_list = os.listdir(snippets_list_path)
    for snippet in snippets_list:
        if snippet == '.yas-parents' or snippet == '.yas-setup.el'\
           or snippet == '.read-me' or snippet == '.DS_Store':
            continue

        snippet_file_path = os.path.join(snippets_list_path, snippet)
        if os.path.isfile(snippet_file_path):
            if_edit = False
            try:
                lines = open(snippet_file_path, 'r').readlines()
            except UnicodeDecodeError:
                continue

            hasKey = False
            for i, line in enumerate(lines):
                if (line.startswith('#key:') or line.startswith('#key :')
                    ) or line.startswith('# key:'):
                    hasKey = True
                    if not line.endswith('->\n'):
                        lines[i] = line[:-1] + '->\n'
                        if_edit = True
                    elif line.endswith('->'):
                        lines[i] = line + '\n'
                        if_edit = True

            if not hasKey:
                lines = lines[:1] + ['#key: {}->\n'.format(snippet)
                                     ] + lines[1:]
                if_edit = True

            if if_edit:
                print(snippet_file_path)
                f = open(snippet_file_path, 'w')
                f.writelines(lines)

    print(
        '================================================================================'
    )
    print(f'finish {snippets_list_path}')
    print(
        '================================================================================'
    )
