#!/bin/bash



################################################################
#                                                              #
#   Installs the dependencies for vim that my .vimrc expects   #
#                                                              #
################################################################



################
# Housekeeping #
################

# If the following directories don't exist, make them:
declare -a paths=(
  ~/bin 
  ~/.vim 
  ~/.vim/colors
  ~/.vim/dependencies
  ~/.oni
)

for p in "${paths[@]}"; do
  if [ ! -d $p ]; then
    echo "Creating directory $p..."
    mkdir $p
  fi
done

# Make sure ~/bin is in PATH
#  If it isn't, add it to the PATH and update ~/.bashrc to make sure it is in the future
case :$PATH: in
  *:$HOME/bin:*)
    ;; # ~/bin is in the path; do nothing
  *)
    # Default case: ~/bin is _not_ in the path; put it there
    echo "~/bin not found in PATH; adding to PATH and appending ~/.bashrc..."
    export PATH=~/bin:$PATH
    echo "export PATH=~/bin:$PATH" >> ~/.bashrc
    ;;
esac

# Keep track of uninstalled dependencies and other messages that the we'll want to see at the end
declare -a post_install_messages=()


#####################
# Copy config files #
#####################

if [ ! -f ~/.vimrc ]; then
  echo ".vimrc not found; linking to ./.vimrc"
  ln ./.vimrc ~/.vimrc
fi

if [ ! -f ~/.oni/config.js ]; then
  echo "Oni config not found; linking to ./oni-config.js"
  ln ./oni-config.js ~/.oni/config.js
fi


############################
# Install the dependencies #
############################

# Ripgrep
ripgrep_dep_message='Uninstalled dependency: ripgrep. To install ripgrep, follow the instructions at https://github.com/BurntSushi/ripgrep'
if [ -z $(which rg) ]; then
  if [ ! -z $(which cargo) ]; then
    echo "Installing ripgrep via cargo..."
    if ! cargo install ripgrep ; then
      echo '  Failed to install ripgrep via cargo. Please try manually.'
      post_install_messages+=("$ripgrep_dep_message")
    fi
  elif [ ! -z $(which brew) ]; then
    echo "Installing ripgrep via brew..."
    if ! brew install ripgrep ; then
      echo '  Failed to install ripgrep via cargo. Please try manually.'
      post_install_messages+=("$ripgrep_dep_message")
    fi
  else
    echo "Not sure how to install ripgrep; skipping..."
    post_install_messages+=("$ripgrep_dep_message")
  fi
else
  echo "ripgrep found; skipping..."
fi


# jq
# TODO: Take a cli flag for the OS
#  It'd be awesome if we could determine the OS without that, though...
#  For now, just assume linux
if [ -z $(which jq) ]; then
  echo "Downloading jq for 64-bit Linux..."
  post_install_messages+=('jq was downloaded for 64-bit Linux. If this is NOT 64-bit Linux, go to https://stedolan.github.io/jq/download/ and download the file to ~/bin/jq')
  # wget -O ~/bin/jq https://github.com/stedolan/jq/releases/download/jq-1.5/jq-linux64
else
  echo "jq found; skipping..."
fi

# Universal ctags
if [ ! -z $(which git) ]; then
  if [ -z $(which uctags) ]; then
    echo "Installing universal ctags for Linux..."
    # Clone the repo if necessary
    if [ ! -d ~/.vim/dependencies/ctags ]; then
      git clone https://github.com/universal-ctags/ctags ~/.vim/dependencies/ctags
    fi

    # Run the installation commands
    # TODO: Make this OS dependent as well
    # For now, run the commands found in https://github.com/universal-ctags/ctags/blob/master/docs/autotools.rst
    cd ~/.vim/dependencies/ctags
    # TODO: Figure out why this didn't hit the `then` when `./autogen.sh` failed...
    if ! {./autogen.sh &&
      \ ./configure --prefix=~/ --program-prefix=u && # compile to uctags for universal ctags
      \ make &&
      \ make install}; then
      # If the installation fails, do the following
      echo "Failed to install universal ctags..."
      post_install_messages+=('Uninstalled dependency: universal ctags. To install universal ctags, follow the instructions at https://github.com/universal-ctags/ctags')
    fi
  else
    echo "Universal ctags found; skipping..."
  fi
else
  echo "Git doesn't seem to be installed. Skipping universal ctags..."
  post_install_messages+=("Uninstalled dependency: universal ctags. Install git and try again.")
fi


##########################
# Post-install messaging #
##########################
echo
echo "Finished installing dependencies."

if [ ${#post_install_messages} -ne 0 ]; then
  echo
  for dep_message in "${post_install_messages[@]}"; do
    echo $dep_message
  done
fi

