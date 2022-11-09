use std::{fs::File, io::BufReader};
use std::io::Read;
use eframe::egui;

pub mod bus;
pub mod cpu;

pub struct Ui();

impl eframe::App for Ui {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Hello world!");
        });
    }
}

fn main() {
    let f = File::open("example.bin").unwrap();
    let mut reader = BufReader::new(f);
    let mut buffer: Vec<u8> = Vec::new();

    reader.read_to_end(&mut buffer).unwrap();

    let bus = bus::BasicBus::try_from(buffer).unwrap();
    let mut cpu6502 = cpu::Cpu::new(bus);

    eframe::run_native("My egui app", eframe::NativeOptions::default(), Box::new(|_cc| Box::new(Ui{})));
}
