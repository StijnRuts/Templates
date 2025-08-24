<?php

namespace App;

use Monolog\Logger as Logger;
use Monolog\Handler\StreamHandler as StreamHandler;

final class Main
{
    public static function page(): void
    {
        $path = $_SERVER["REQUEST_URI"];

        $log = new Logger('name');
        $log->pushHandler(new StreamHandler('app.log', Logger::INFO));
        $log->info($path);

        echo '<h1>'.htmlspecialchars($path).'</h1>';
    }
}
