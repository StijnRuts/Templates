<?php
declare(strict_types=1);

use PHPUnit\Framework\TestCase;

final class AppTest extends TestCase
{
    public function testAddition(): void
    {
        $this->assertSame(1+1, 2);
    }
}
