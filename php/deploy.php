<?php
namespace Deployer;

require 'recipe/common.php';

set('repository', '');

add('shared_files', []);
add('shared_dirs', []);
add('writable_dirs', []);

after('deploy:failed', 'deploy:unlock');
