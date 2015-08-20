# config valid only for current version of Capistrano
lock '3.4.0'

set :application, 'e_chat_server'
set :repo_url, 'git@github.com:dsaveliev/e_chat_server.git'

set :deploy_to, '/srv/e_chat_server'
set :user, 'root'

# Default branch is :master
# ask :branch, `git rev-parse --abbrev-ref HEAD`.chomp

# Default deploy_to directory is /var/www/my_app_name
# set :deploy_to, '/var/www/my_app_name'

# Default value for :scm is :git
# set :scm, :git

# Default value for :format is :pretty
set :format, :pretty

# Default value for :log_level is :debug
set :log_level, :debug

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
# set :linked_files, fetch(:linked_files, []).push('config/database.yml', 'config/secrets.yml')

# Default value for linked_dirs is []
# set :linked_dirs, fetch(:linked_dirs, []).push('log', 'tmp/pids', 'tmp/cache', 'tmp/sockets', 'vendor/bundle', 'public/system')

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for keep_releases is 5
set :keep_releases, 5

namespace :deploy do
  desc 'Migrate'
  task :migrate do
    on roles(:app), in: :sequence, wait: 5 do
      within release_path do
        execute '/srv/e_chat_server/current/bin/migrate.sh'
      end
    end
  end

  after :finished, :restart do
    on roles(:app), in: :sequence, wait: 5 do
      within release_path do
        execute :cp, '/srv/shared/sys.config rel/sys.config'
        execute :make, 'all'
        begin execute '/srv/e_chat_server/current/_rel/e_chat_server_release/bin/e_chat_server_release stop'; rescue; end
        execute '/srv/e_chat_server/current/_rel/e_chat_server_release/bin/e_chat_server_release start'
        execute :service, 'nginx restart'
      end
    end
  end

end
