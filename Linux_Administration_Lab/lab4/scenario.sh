# 1
# https://www.2daygeek.com/install-development-tools-on-ubuntu-debian-arch-linux-mint-fedora-centos-rhel-opensuse/
sudo apt-get update
sudo apt-get install build-essential

#2
cd ~
cp /media/sf_Kubuntu-22/drive-download-20221215T160236Z-001/bastet-0.43.tgz ~/
tar -zxvf bastet-0.43.tgz

make 
# Ломается на "Block.hpp:22:10: fatal error: boost/array.hpp: No such file or directory"
# https://stackoverflow.com/questions/12241152/boost-no-such-file-or-directory
# sudo apt-get install libboost-dev

make
# Block.hpp:23:10: fatal error: curses.h: No such file or directory
sudo apt-get install libncurses-dev

make
# /usr/bin/ld: cannot find -lboost_program_options: No such file or directory
sudo apt update
sudo apt upgrade
# не особо помогло

# перезагрузка системы
# не помогло

# https://stackoverflow.com/questions/12578499/how-to-install-boost-on-ubuntu
# Попробуем:
sudo apt-get install libboost-all-dev
make
# заработало

# Добавляем в /usr/bin
# >> Makefile:
# install:
#	sudo cp ./bastet /usr/bin/
#	sudo chmod a+x /usr/bin/bastet

make install

# 3
apt list --installed > task3.log

# 4
apt depends gcc > task4_1.log
# 
# Не существует пакета libgcc с чистым именем
# Возьмем для примера пакет libgcc-11-dev
apt rdepends libgcc-11-dev > task4_2.log

# 5
mkdir ~/localrepo
cp /media/sf_Kubuntu-22/drive-download-20221215T160236Z-001/checkinstall-1.6.2-3.el6.1.x86_64.rpm ~/localrepo/
# установим createrepo, такой пакет не найден на kubuntu (позже окажется не нужным)
sudo apt install createrepo-c

# https://askubuntu.com/questions/170348/how-to-create-a-local-apt-repository
# https://wiki.ubuntu.com/RussianDocumentation/CreatingLocalReprositoryHowto
# Для Kubuntu изменение репозиториев происходит через файл /etc/apt/sources.list
# https://phoenixnap.com/kb/install-rpm-packages-on-ubuntu
# с помощью alien преобразуем rpm в deb
sudo alien checkinstall-1.6.2-3.el6.1.x86_64.rpm
# сделаем резервную копию файла
sudo cp /etc/apt/sources.list /etc/apt/sources.list.backup

# Удалим всё кроме .deb файла в папке localrepo
# Теперь будем идти по инструкции
cd ~/localrepo
dpkg-scanpackages . /dev/null | gzip -9c > Packages.gz

sudo echo "deb file:/home/legend/localrepo/ ./" >> /etc/apt/sources.list

# были проблемы:
# N: Загрузка выполняется от лица суперпользователя без ограничений песочницы, так как файл «/home/legend/localrepo/./InRelease» недоступен для пользователя «_apt». - pkgAcquire::Run (13: Permission denied)
# E: Репозиторий «file:/home/legend/localrepo ./ Release» не содержит файла Release.
# N: Обновление из этого репозитория нельзя выполнить безопасным способом, поэтому по умолчанию он отключён.
# N: Информацию о создании репозитория и настройках пользователя смотрите в справочной странице apt-secure(8).

# Немного решилось добавлением: [trusted=yes]:
deb [trusted=yes] file:/home/legend/localrepo/ ./
# Однако не до конца смогло решить проблему:
# https://www.linux.org.ru/forum/general/11724893

# N: Загрузка выполняется от лица суперпользователя без ограничений песочницы, так как файл «/home/legend/localrepo/./InRelease» недоступен для пользователя «_apt». - pkgAcquire::Run (13: Permission denied)
# E: Не удалось получить file:/home/legend/localrepo/./Packages  Файл не найден - /home/legend/localrepo/./Packages (2: No such file or directory)
# E: Некоторые индексные файлы скачать не удалось. Они были проигнорированы, или вместо них были использованы старые версии.

# Сделал через: 
dpkg-scanpackages -m . > Packages
# И что-то как-то заработало

# Установка сработала

# 6
apt list > task6.log

# 7
# Изменим /etc/apt/sources.list таким образом, чтобы оставить только локальный репозиторий
# Вроде, заработало на практике...

legend@legend-VirtualBox:~$ apt list | grep -v '\[установлен'

WARNING: apt does not have a stable CLI interface. Use with caution in scripts.

Вывод списка…
fortunes-ru/неизвестно 1.52-2 all
linux-image-5.15.0-43-generic/now 5.15.0-43.46 amd64 [остались файлы настроек]
linux-image-5.15.0-48-generic/now 5.15.0-48.54 amd64 [остались файлы настроек]
linux-modules-5.15.0-43-generic/now 5.15.0-43.46 amd64 [остались файлы настроек]
linux-modules-5.15.0-48-generic/now 5.15.0-48.54 amd64 [остались файлы настроек]
linux-modules-extra-5.15.0-43-generic/now 5.15.0-43.46 amd64 [остались файлы настроек]
linux-modules-extra-5.15.0-48-generic/now 5.15.0-48.54 amd64 [остались файлы настроек]
legend@legend-VirtualBox:~$

# 8
# Для Kubuntu не нужно использовать alien

legend@legend-VirtualBox:~/localrepo$ sudo apt install fortunes-ru
Чтение списков пакетов… Готово
Построение дерева зависимостей… Готово
Чтение информации о состоянии… Готово         
Некоторые пакеты не могут быть установлены. Возможно, то, что вы просите,
неосуществимо, или же вы используете нестабильную версию дистрибутива, где
запрошенные вами пакеты ещё не созданы или были удалены из Incoming.
Следующая информация, возможно, вам поможет:

Следующие пакеты имеют неудовлетворённые зависимости:
 fortunes-ru : Зависит: fortune-mod (>= 9708-12) но он не может быть установлен
E: Невозможно исправить ошибки: у вас зафиксированы сломанные пакеты.

###################

legend@legend-VirtualBox:~/localrepo$ sudo dpkg -i fortunes-ru_1.52-2_all.deb 
Выбор ранее не выбранного пакета fortunes-ru.
(Чтение базы данных … на данный момент установлено 262108 файлов и каталогов.)
Подготовка к распаковке fortunes-ru_1.52-2_all.deb …
Распаковывается fortunes-ru (1.52-2) …
dpkg: зависимости пакетов не позволяют настроить пакет fortunes-ru:
 fortunes-ru зависит от fortune-mod (>= 9708-12), однако:
  Пакет fortune-mod не установлен.

dpkg: ошибка при обработке пакета fortunes-ru (--install):
 проблемы зависимостей — оставляем не настроенным
При обработке следующих пакетов произошли ошибки:
 fortunes-ru

#######################
legend@legend-VirtualBox:~$ sudo apt install fortune-mod
Чтение списков пакетов… Готово
Построение дерева зависимостей… Готово
Чтение информации о состоянии… Готово         
Вы можете запустить «apt --fix-broken install» для исправления этих ошибок.
Следующие пакеты имеют неудовлетворённые зависимости:
 fortune-mod : Зависит: librecode0 (>= 3.6) но он не будет установлен
E: Неудовлетворённые зависимости. Попытайтесь выполнить «apt --fix-broken install», не указывая имени пакета (или указав решение).

legend@legend-VirtualBox:~$ sudo apt install librecode0
Чтение списков пакетов… Готово
Построение дерева зависимостей… Готово
Чтение информации о состоянии… Готово         
Вы можете запустить «apt --fix-broken install» для исправления этих ошибок.
Следующие пакеты имеют неудовлетворённые зависимости:
 fortunes-ru : Зависит: fortune-mod (>= 9708-12) но он не будет установлен
E: Неудовлетворённые зависимости. Попытайтесь выполнить «apt --fix-broken install», не указывая имени пакета (или указав решение).

# 9
# https://habr.com/ru/post/28366/